# spinla

R-INLA for SPI data fusion (observations + ERA5 reanalysis)

## Input data

### Temporal coverage

- [] 1981 - 2022
- [] 1991 - 2022

### Station data

Two possible data sources for precipitation time series:

- [] marsMET (JRC)

- [] SCIA (ISPRA)
  
- [NO] ARCIS: **data must be provided by the data owners, a single data source from which to retrieve the data is not available** 

### Covariates

- [] Indici di stabilità anticiclonica - variabilità Anticiclone Sub Tropicale

  *Seguendo Davies et al., 1997 potremmo calcolare "the overall mean frequency and (b) standard deviation of days per half month with sea level pressure >1020 mb at each grid point" . Questo indice ci direbbe quando l'anticiclone si estende sull'area di interesse o anche nell'area più grande.*

- [] StormTrack Atlantica - variabilità ciclonica
  
*La ciclonicità è sicuramente connessa all'intensità e alla tipologia della precipitazione. Potremmo usare l'indice cosiddetto di storm track "... storm track intensity primarily using high-pass-filtered 300-hPa meridional velocity (henceforth referred to as υ′) variance", che tiene conto dell'intensità e la posizione-passaggio di strutture cicloniche ( Wallace et al, 1998, Chang and Fu, 2002).*

- [] Instabilità convettiva atmosferica - propensione alla convezione
  
*Esistono numerosi indici calcolabili da grandezze atmosferiche per identificare il potenziale convettivo di un determinato stato dell'atmosfera in una precisa località. Ma userei quelli semplificati, basati sulla temperatura. Uno interessante è il DeltaThetaEquivalente che misura la differenza di temperatura potenziale equivalente tra 2 strati dell'atmosfera, tra 500hPa e 850hPa. In pratica laddove questo gradiente è positivo si ha una inibizione della convezione,  mentre quando è positivo si ha una propensione. Ovviamente non è detto che dove ci sia propensione la convezione parta realmente. Ma comunque a scala più che mensile dovrebbe essere significativo il suo contributo magari dal cumulato della sua sola parte positiva.  Anche il campo delle fulminazioni funzionerebbe, ma sarebbe meno pratico da avere bello e pronto, al contrario del campo di DeltaThetaE.*


## Study Domain

## SPI time series

- [] SPI-3
- [] SPI-6

## References

Chang, E. K. M., and Y. Fu, 2002: **Interdecadal Variations in Northern Hemisphere Winter Storm Track Intensity**. J. Climate, 15, 642–658, https://doi.org/10.1175/1520-0442(2002)015<0642:IVINHW>2.0.CO;2.
https://journals.ametsoc.org/view/journals/clim/15/6/1520-0442_2002_015_0642_ivinhw_2.0.co_2.xml 
 
Davis, R. E., B. P. Hayden, D. A. Gay, W. L. Phillips, and G. V. Jones, 1997: **The North Atlantic Subtropical Anticyclone**. J. Climate, 10, 728–744, https://doi.org/10.1175/1520-0442(1997)010<0728:TNASA>2.0.CO;2. 
https://journals.ametsoc.org/view/journals/clim/10/4/1520-0442_1997_010_0728_tnasa_2.0.co_2.xml 
 
Wallace, J. M., G. Lim, and M. L. Blackmon, 1988: **Relationship between Cyclone Tracks, Anticyclone Tracks and Baroclinic Waveguides**. J. Atmos. Sci., 45, 439–462,https://doi.org/10.1175/1520-0469(1988)045<0439:RBCTAT>2.0.CO;2. 
https://journals.ametsoc.org/view/journals/atsc/45/3/1520-0469_1988_045_0439_rbctat_2_0_co_2.xml




  


