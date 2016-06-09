## Test environments
* local Windows 7 Pro, R 3.1.3
* Linix - Ubuntu (via Travis)
* passes devtools::build_win()

## R CMD check results
*No ERRORs.  
*One WARNING: 
checking data for ASCII and uncompressed saves ... WARNING
  
  Note: significantly better compression could be obtained
        by using R CMD build --resave-data
                old_size new_size compress
  HI.coast.rda      78Kb     62Kb       xz
  WA.cities.rda     44Kb     35Kb       xz
  WA.rda           227Kb    130Kb       xz
  WY.rda            20Kb     16Kb       xz

**This warning will be remedied during final build**

## Downstream dependencies
None known
