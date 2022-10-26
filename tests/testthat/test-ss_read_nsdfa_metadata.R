# ss_read_nsdfa_metadata() ------------------------------------------------

test_that("ss_read_nsdfa_metadata() reads in all data", {
  expect_equal(nrow(nsdfa), 51)
  expect_equal(ncol(nsdfa), 30)
})


test_that("ss_read_nsdfa_metadata() reads in correct classes", {
  expect_equal(class(nsdfa$Depl_Date), "Date")
  expect_equal(class(nsdfa$Recv_Date), "Date")
})

# spot checks
test_that("ss_read_nsdfa_metadata() corrects waterbody names", {

  expect_equal(nrow(filter(nsdfa, Waterbody == "Pipers lake")), 0)
  expect_equal(nrow(filter(nsdfa, Waterbody == "Piper Lake")), 1)

  expect_equal(nrow(filter(nsdfa, Waterbody == "St Marys Bay")), 0)
  expect_equal(nrow(filter(nsdfa, Waterbody == "St. Mary's Bay")), 7)

  expect_equal(nrow(filter(nsdfa, Waterbody == "St Margarets Bay")), 0)
  expect_equal(nrow(filter(nsdfa, Waterbody == "St. Margarets Bay")), 7)

  expect_equal(nrow(filter(nsdfa, Waterbody == "St Anns Bay")), 0)
  expect_equal(nrow(filter(nsdfa, Waterbody == "St. Ann's Bay")), 1)

  expect_equal(nrow(filter(nsdfa, Waterbody == "St Peters Inlet")), 0)
  expect_equal(nrow(filter(nsdfa, Waterbody == "St Peter's Inlet")), 1)

  expect_equal(
    nrow(filter(nsdfa, Waterbody == "Tor Bay" & Station_Name == "Bald Rock")), 0
  )
  expect_equal(
    nrow(filter(nsdfa, Waterbody == "Whitehead Harbour" &
                  Station_Name == "Bald Rock")), 2
  )

})


test_that("ss_read_nsdfa_metadata() corrects station names", {

  expect_equal(nrow(filter(nsdfa, Station_Name == "Pipers lake")), 0)
  expect_equal(nrow(filter(nsdfa, Station_Name == "Piper Lake")), 1)

  expect_equal(nrow(filter(nsdfa, Station_Name == "Shut-in Island")), 0)
  expect_equal(nrow(filter(nsdfa, Station_Name == "Shut-In Island")), 3)

  expect_equal(nrow(filter(nsdfa, Station_Name == "Owls head")), 0)
  expect_equal(nrow(filter(nsdfa, Station_Name == "Owls Head")), 1)

})
