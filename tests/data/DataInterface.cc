#define CATCH_CONFIG_MAIN

#ifndef EMP_TRACK_MEM
#define EMP_TRACK_MEM
#endif

#include "third-party/Catch/single_include/catch.hpp"

#include "data/DataNode.h"
#include "data/DataManager.h"
#include "data/DataInterface.h"
#include "data/DataFile.h"

#include <algorithm>
#include <cmath>
#include <fstream>
#include <iostream>
#include <iterator>
#include <string>


TEST_CASE("Test DataInterface", "[data]") {
    auto * di = emp::MakeDataInterface<double, emp::data::Current, emp::data::Range, emp::data::Pull, emp::data::Log>();
    delete di;

    emp::DataNode<double, emp::data::Current, emp::data::Range> node;
    node.Add(5.5, .6); // Put in some test data, since we can't add through the interface

    auto * di2(&node);
    REQUIRE(di2->GetTotal() == 6.1);
    REQUIRE(di2->GetMin() == .6);
    REQUIRE(di2->GetMax() == 5.5);
    REQUIRE(di2->GetMean() == 3.05);

    emp::DataNode<double, emp::data::Current, emp::data::Range, emp::data::Stats> node2;
    node2.Add(5.5, .6); // Put in some test data, since we can't add through the interface

    auto * di3(&node2);

    REQUIRE(di3->GetTotal() == 6.1);
    REQUIRE(di3->GetMin() == .6);
    REQUIRE(di3->GetMax() == 5.5);
    REQUIRE(di3->GetMean() == 3.05);
    REQUIRE(di3->GetVariance() == Approx(6.0025));
    REQUIRE(di3->GetStandardDeviation() == Approx(2.45));
    REQUIRE(di3->GetSkew() == 0);
    REQUIRE(di3->GetKurtosis() == -2);
}