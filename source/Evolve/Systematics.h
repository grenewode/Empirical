/**
 *  @note This file is part of Empirical, https://github.com/devosoft/Empirical
 *  @copyright Copyright (C) Michigan State University, MIT Software license; see doc/LICENSE.md
 *  @date 2017
 *
 *  @file  Systematics.h
 *  @brief Track genotypes, species, clades, or lineages of organisms in a world.
 *
 *
 *  @todo Technically, we don't need to keep the ancestors in a set in order to track a lineage...
 *        If we delete all of their descendants they should automaticaly be deleted.
 *  @todo We should provide an option to back up systematics data to a file so that it doesn't all
 *        need to be kept in memory, especially if we're only doing post-analysis.
 *  @todo We should have a systematics interface that will convert organisms into a different
 *        (internal) type to track.  This would allow us to have an arbitrary number of systematics
 *        trackers in world, all of which take an organism, but store different types of data.
 */


#ifndef EMP_EVO_SYSTEMATICS_H
#define EMP_EVO_SYSTEMATICS_H

#include <ostream>
#include <set>
#include <unordered_set>

#include "../base/Ptr.h"
#include "../tools/info_theory.h"
#include "../tools/set_utils.h"

namespace emp {

  /// @brief A Taxon represents a type of organism in a phylogeny.
  /// @param ORG_INFO The information type associated with an organism, used to categorize it.
  ///
  /// Genotypes are the most commonly used Taxon; in general taxa can be anything from a shared
  /// genome sequence, a phenotypic trait, or a even a position in the world (if you want to
  /// track an evolutionary pathway)

  template <typename ORG_INFO>
  class Taxon {
  private:
    using this_t = Taxon<ORG_INFO>;
    using info_t = ORG_INFO;

    size_t id;                ///<  ID for this Taxon (Unique within this Systematics)
    const info_t info;        ///<  Details for the organims associated within this taxanomic group.
    const Ptr<this_t> parent; ///<  Pointer to parent group (nullptr if injected)
    size_t num_orgs;          ///<  How many organisms currently exist of this group?
    size_t tot_orgs;          ///<  How many organisms have ever existed of this group?
    size_t num_offspring;     ///<  How many direct offspring groups exist from this one.
    size_t depth;             ///<  How deep in tree is this node? (Root is 0)

  public:
    Taxon(size_t _id, const info_t & _info, Ptr<this_t> _parent=nullptr)
     : id (_id), info(_info), parent(_parent), num_orgs(0), tot_orgs(0), num_offspring(0)
     , depth(parent ? (parent->depth+1) : 0) { ; }
    Taxon(const Taxon &) = delete;
    Taxon(Taxon &&) = default;
    Taxon & operator=(const Taxon &) = delete;
    Taxon & operator=(Taxon &&) = default;

    /// Get a unique ID for this taxon; IDs are assigned sequentially, so newer taxa have higher IDs.
    size_t GetID() const { return id; }

    /// Retrieve the tracked info associated with this Taxon.
    const info_t & GetInfo() const { return info; }

    /// Retrieve a pointer to the parent Taxon.
    Ptr<this_t> GetParent() const { return parent; }

    /// Get the number of living organisms currently associated with this Taxon.
    size_t GetNumOrgs() const { return num_orgs; }

    /// Get the total number of organisms that have ever lived associated with this Taxon
    size_t GetTotOrgs() const { return tot_orgs; }

    /// Get the number of taxa that were produced by organisms from this Taxon.
    size_t GetNumOff() const { return num_offspring; }

    /// Get the number of taxanomic steps since the ancestral organism was injected into the World.
    size_t GetDepth() const { return depth; }

    /// Add a new organism to this Taxon.
    void AddOrg() { ++num_orgs; ++tot_orgs; }

    /// Add a new offspring Taxon to this one.
    void AddOffspring() { ++num_offspring; }

    /// Remove an organism from this Taxon (after it dies).
    /// Removals must return true if the taxon needs to continue; false if it should deactivate.
    bool RemoveOrg() {
      emp_assert(num_orgs > 0, num_orgs);
      --num_orgs;

      // If we are out of BOTH organisms and offspring, this Taxon should deactivate.
      return num_orgs;
    }

    /// Remove and offspring taxa after its entire sub-tree has died out (pruning)
    bool RemoveOffspring() {
      emp_assert(num_offspring > 0);
      --num_offspring;

      // If we are out of BOTH offspring and organisms, this Taxon should deactivate.
      return num_orgs || num_offspring;
    }
  };

  /// @brief A tool to track phylogenetic relationships among organisms.
  /// The systematics class tracks the relationships among all organisms based on the INFO_TYPE
  /// provided.  If an offspring has the same value for INFO_TYPE as its parent, it is grouped into
  /// the same taxon.  Otherwise a new Taxon is created and the old one is used as its parent in
  /// the phylogeny.  If the provided INFO_TYPE is the organsism's genome, a traditional phylogeny
  /// is formed, with genotypes.  If the organism's behavior/task set is used, then organisms are
  /// grouped by phenotypes.  If the organsims's position is used, the evolutionary path through
  /// space is tracked.  Any other aspect of organisms can be tracked this way as well.
  template <typename ORG_INFO>
  class Systematics {
  private:
    using taxon_t = Taxon<ORG_INFO>;
    using hash_t = typename Ptr<taxon_t>::hash_t;

    bool store_active;     ///< Store all of the currently active taxa?
    bool store_ancestors;  ///< Store all of the direct ancestors from living taxa?
    bool store_outside;    ///< Store taxa that are extinct with no living descendants?
    bool archive;          ///< Set to true if we are supposed to do any archiving of extinct taxa.

    std::unordered_set< Ptr<taxon_t>, hash_t > active_taxa;   ///< A set of all living taxa.
    std::unordered_set< Ptr<taxon_t>, hash_t > ancestor_taxa; ///< A set of all dead, ancestral taxa.
    std::unordered_set< Ptr<taxon_t>, hash_t > outside_taxa;  ///< A set of all dead taxa w/o descendants.

    // Stats about active taxa... (totals are across orgs, not taxa)
    size_t org_count;           ///< How many organisms are currently active?
    size_t total_depth;         ///< Sum of taxa depths for calculating average.
    size_t num_roots;           ///< How many distint injected ancestors are currently in population?

    size_t next_id;             ///< What ID value should the next new taxon have?
    mutable Ptr<taxon_t> mrca;  ///< Most recent common ancestor in the population.

    /// Called wheneven a taxon has no organisms AND no descendants.
    void Prune(Ptr<taxon_t> taxon);

    /// Called when an offspring taxa has been deleted.
    void RemoveOffspring(Ptr<taxon_t> taxon);

    /// Called when there are no more living members of a taxon.  There may be descendants.
    void MarkExtinct(Ptr<taxon_t> taxon);

  public:
    /**
     * Contructor for Systematics; controls what information should be stored.
     * @param store_active     Should living organisms' taxa be tracked? (typically yes!)
     * @param store_ancestors  Should ancestral organims' taxa be maintained?  (yes for lineages!)
     * @param store_outside    Should all dead taxa be maintained? (typically no; it gets BIG!)
     */
    Systematics(bool _active=true, bool _anc=true, bool _all=false)
      : store_active(_active), store_ancestors(_anc), store_outside(_all)
      , archive(store_ancestors || store_outside)
      , active_taxa(), ancestor_taxa(), outside_taxa()
      , org_count(0), total_depth(0), num_roots(0), next_id(0)
      , mrca(nullptr) { ; }
    Systematics(const Systematics &) = delete;
    Systematics(Systematics &&) = default;
    ~Systematics() {
      for (auto x : active_taxa) x.Delete();
      for (auto x : ancestor_taxa) x.Delete();
      for (auto x : outside_taxa) x.Delete();
      active_taxa.clear();
      ancestor_taxa.clear();
      outside_taxa.clear();
    }

    /// Are we storing all taxa that are still alive in the population?
    bool GetStoreActive() const { return store_active; }

    /// Are we storing all taxa that are the ancestors of living organims in the population?
    bool GetStoreAncestors() const { return store_ancestors; }

    /// Are we storing all taxa that have died out, as have all of their descendants.
    bool GetStoreOutside() const { return store_outside; }

    /// Are we storing any taxa types that have died out?
    bool GetArchive() const { return archive; }

    /// How many taxa are still active in the population?
    size_t GetNumActive() const { return active_taxa.size(); }

    /// How many taxa are ancestors of living organisms (but have died out themselves)?
    size_t GetNumAncestors() const { return ancestor_taxa.size(); }

    /// How many taxa are stored that have died out, as have their descendents?
    size_t GetNumOutside() const { return outside_taxa.size(); }

    /// How many taxa are in the current phylogeny?
    size_t GetTreeSize() const { return GetNumActive() + GetNumAncestors(); }

    /// How many taxa are stored in total?
    size_t GetNumTaxa() const { return GetTreeSize() + GetNumOutside(); }

    /// How many living organisms are currently being tracked?
    size_t GetTotalOrgs() const { return org_count; }

    /// How many independent trees are being tracked?
    size_t GetNumRoots() const { return num_roots; }

    /// What is the average phylogenetic depth of organisms in the population?
    double GetAveDepth() const { return ((double) total_depth) / (double) org_count; }

    /// Request a pointer to the Most-Recent Common Ancestor for the population.
    Ptr<taxon_t> GetMRCA() const;

    /// Request the depth of the Most-Recent Common Ancestor; return -1 for none.
    int GetMRCADepth() const;

    /// Add information about a new organism, including its stored info and parent's taxon;
    /// return a pointer for the associated taxon.
    Ptr<taxon_t> AddOrg(const ORG_INFO & info, Ptr<taxon_t> cur_taxon=nullptr);

    /// Remove an instance of an organism; track when it's gone.
    bool RemoveOrg(Ptr<taxon_t> taxon);

    /// Climb up a lineage...
    Ptr<taxon_t> Parent(Ptr<taxon_t> taxon) const;

    /// Print details about the Systematics manager.
    void PrintStatus(std::ostream & os=std::cout) const;

    /// Print whole lineage.
    void PrintLineage(Ptr<taxon_t> taxon, std::ostream & os=std::cout) const;

    /// Calculate the genetic diversity of the population.
    double CalcDiversity();
  };

  // =============================================================
  // ===                                                       ===
  // ===  Out-of-class member function definitions from above  ===
  // ===                                                       ===
  // =============================================================

  // Should be called wheneven a taxon has no organisms AND no descendants.
  template <typename ORG_INFO>
  void Systematics<ORG_INFO>::Prune(Ptr<taxon_t> taxon) {
    RemoveOffspring( taxon->GetParent() );           // Notify parent of the pruning.
    if (store_ancestors) ancestor_taxa.erase(taxon); // Clear from ancestors set (if there)
    if (store_outside) outside_taxa.insert(taxon);   // Add to outside set (if tracked)
    else taxon.Delete();                             //  ...or else get rid of it.
  }

  template <typename ORG_INFO>
  void Systematics<ORG_INFO>::RemoveOffspring(Ptr<taxon_t> taxon) {
    if (!taxon) { num_roots--; return; }               // Offspring was root; remove and return.
    bool still_active = taxon->RemoveOffspring();      // Taxon still active w/ 1 fewer offspring?
    if (!still_active) Prune(taxon);                   // If out of offspring, remove from tree.

    // If the taxon is still active AND the is the current mrca AND now has only one offspring,
    // clear the MRCA for lazy re-evaluation later.
    else if (taxon == mrca && taxon->GetNumOff() == 1) mrca = nullptr;
  }

  // Mark a taxon extinct if there are no more living members.  There may be descendants.
  template <typename ORG_INFO>
  void Systematics<ORG_INFO>::MarkExtinct(Ptr<taxon_t> taxon) {
    emp_assert(taxon);
    emp_assert(taxon->GetNumOrgs() == 0);

    if (store_active) active_taxa.erase(taxon);
    if (!archive) {   // If we don't archive taxa, delete them.
      taxon.Delete();
      return;
    }

    if (store_ancestors) ancestor_taxa.insert(taxon);  // Move taxon to ancestors...
    if (taxon->GetNumOff() == 0) Prune(taxon);         // ...and prune from there if needed.
  }


  // Request a pointer to the Most-Recent Common Ancestor for the population.
  template <typename ORG_INFO>
  Ptr<Taxon<ORG_INFO>> Systematics<ORG_INFO>::GetMRCA() const {
    if (!mrca && num_roots == 1) {  // Determine if we need to calculate the MRCA.
      // First, find a candidate among the living taxa.  Only taxa that have one offsrping
      // can be on the line-of-descent to the MRCA, so anything else is a good start point.
      // There must be at least one!  Stop as soon as we find a candidate.
      Ptr<taxon_t> candidate(nullptr);
      for (auto x : active_taxa) {
        if (x->GetNumOff() != 1) { candidate = x; break; }
      }

      // Now, trace the line of descent, updating the candidate as we go.
      Ptr<taxon_t> test_taxon = candidate->GetParent();
      while (test_taxon) {
        emp_assert(test_taxon->GetNumOff() >= 1);
        if (test_taxon->GetNumOff() > 1) candidate = test_taxon;
        test_taxon = test_taxon->GetParent();
      }
      mrca = candidate;
    }
    return mrca;
  }

  // Request the depth of the Most-Recent Common Ancestor; return -1 for none.
  template <typename ORG_INFO>
  int Systematics<ORG_INFO>::GetMRCADepth() const {
    GetMRCA();
    if (mrca) return mrca->GetDepth();
    return -1;
  }

  // Add information about a new organism, including its stored info and parent's taxon;
  // return a pointer for the associated taxon.
  template <typename ORG_INFO>
  Ptr<Taxon<ORG_INFO>> Systematics<ORG_INFO>::AddOrg(const ORG_INFO & info, Ptr<taxon_t> cur_taxon) {
    emp_assert( !cur_taxon || Has(active_taxa, cur_taxon) );

    // Update stats
    org_count++;                  // Keep count of how many organisms are being tracked.

    // If this organism needs a new taxon, build it!
    if (!cur_taxon || cur_taxon->GetInfo() != info) {
      auto parent_taxon = cur_taxon;                               // Provided taxon is parent.
      if (!parent_taxon) {                                         // No parnet -> NEW tree
        num_roots++;                                               // ...track extra root.
        mrca = nullptr;                                            // ...nix old common ancestor
      }
      cur_taxon = NewPtr<taxon_t>(++next_id, info, parent_taxon);  // Build new taxon.
      if (store_active) active_taxa.insert(cur_taxon);             // Store new taxon.
      if (parent_taxon) parent_taxon->AddOffspring();              // Track tree info.
    }

    cur_taxon->AddOrg();                    // Record the current organism in its taxon.
    total_depth += cur_taxon->GetDepth();   // Track the total depth (for averaging)
    return cur_taxon;                       // Return the taxon used.
  }

  // Remove an instance of an organism; track when it's gone.
  template <typename ORG_INFO>
  bool Systematics<ORG_INFO>::RemoveOrg(Ptr<taxon_t> taxon) {
    emp_assert(taxon);

    // Update stats
    org_count--;
    total_depth -= taxon->GetDepth();

    // emp_assert(Has(active_taxa, taxon));
    const bool active = taxon->RemoveOrg();
    if (!active) MarkExtinct(taxon);

    return active;
  }

  // Climb up a lineage...
  template <typename ORG_INFO>
  Ptr<Taxon<ORG_INFO>> Systematics<ORG_INFO>::Parent(Ptr<taxon_t> taxon) const {
    emp_assert(taxon);
    emp_assert(Has(active_taxa, taxon));
    return taxon->GetParent();
  }

  // Print details about the Systematics manager.
  template <typename ORG_INFO>
  void Systematics<ORG_INFO>::PrintStatus(std::ostream & os) const {
    os << "Systematics Status:\n";
    os << " store_active=" << store_active
       << " store_ancestors=" << store_ancestors
       << " store_outside=" << store_outside
       << " archive=" << archive
       << " next_id=" << next_id
       << std::endl;
    os << "Active count:   " << active_taxa.size();
    for (const auto & x : active_taxa) {
      os << " [" << x->GetID() << "|" << x->GetNumOrgs() << "," << x->GetNumOff() << "|"
         << ((bool) x->GetParent()) << "]";
    }
    os << std::endl;

    os << "Ancestor count: " << ancestor_taxa.size();
    for (const auto & x : ancestor_taxa) {
      os << " [" << x->GetID() << "|" << x->GetNumOrgs() << "," << x->GetNumOff() << "|"
         << ((bool) x->GetParent()) << "]";
    }
    os << std::endl;

    os << "Outside count:  " << outside_taxa.size();
    for (const auto & x : outside_taxa) {
      os << " [" << x->GetID() << "|" << x->GetNumOrgs() << "," << x->GetNumOff() << "|"
         << ((bool) x->GetParent()) << "]";
    }
    os << std::endl;
  }

  // Print whole lineage.
  template <typename ORG_INFO>
  void Systematics<ORG_INFO>::PrintLineage(Ptr<taxon_t> taxon, std::ostream & os) const {
    os << "Lineage:\n";
    while (taxon) {
      os << taxon->GetInfo() << std::endl;
      taxon = taxon->GetParent();
    }
  }

  // Calculate the genetic diversity of the population.
  template <typename ORG_INFO>
  double Systematics<ORG_INFO>::CalcDiversity() {
    return emp::Entropy(active_taxa, [](Ptr<taxon_t> x){ return x->GetNumOrgs(); }, org_count);
  }

}

#endif
