********************************************************************************
*						Medium Blog Post on Visualization using Stata		   * 
*																 			   *
*  Purpose:  			Generating a Multi Panel Area Chart using Stata        *
*  Author: 				Fahad Mirza 							 			   *
*  Editor:				Fahad Mirza 							 			   *
*  Dated:  				September 30, 2025									   *
*  Last update: 		September 30, 2025									   *
*																 			   *
********************************************************************************


	* Loading the dataset
	use "./sample_area_chart_data.dta", clear
		
	
	* Generating a serial number for reshaping data to long form
	generate num = _n , before(Economy)
	reshape long group_, i(num) j(topic) string
	
	
	* Generating numeric topic group number to sort data according to how
	* we want to present data
	egen topic_group = group(topic)
	
	
	* Creating average by topic
	rename group_ Overall
	bysort topic_group: egen double topic_overall = mean(Overall)
	
	
	* Sorting the data by topic group number and their overall score in ascending order
	sort topic_group Overall, stable
	order Overall, before(topic_overall)
	
	
	* Generating maximum achievable score
	generate hundred = 100
	
	
	* Generating the X-Axis on which we will plot each economy that was sorted by their score
	* within their topic. This includes gaps, whose size is based on the local "jump".
	
		* To change the size of gap between plots, change the value of the local named "jump"
		* higher the value of "jump", bigger the gap between topics
		local jump = 5
		
		generate x_axis = 1 
		replace x_axis = 0 if _n == 1
		
		sort topic_group, stable
		by topic_group: generate jump = `jump' if _n == 1 
		replace jump = . if _n == 1
		
		replace x_axis = x_axis + jump if !missing(jump)
		replace x_axis = sum(x_axis)
		drop jump 
	
	
	* Store number of unique economies in a local
	quietly: levelsof Economy, local(economy)
	local economies : word count `economy'
	
	
	* Store the center point of area bar chart for each topic in a local
	local center = (`economies' - 1) / 2
	
	
	* Topic text position indicator
	* This allows us to move topic names vertically
	* It also allows you to individually adjust gap between 2 words placed on 2 lines
	local txta = 9
	local txtb = 6.5

	
	* Local for opacity of non-neutral colors (c_opacity) and grays (g_opacity)
	local c_opacity 50
	local g_opacity 20

	
	* Topic title size
	local topic_size 0.7

	
	* Initiating a loop that will code the stacked area chart where colored portion
	* depicts the scores achieved while the gray area shows the unachieved portion.
	* The loop runs over each topic.
	local offset = 0
	local j = 1
	
	
	* Line width for mean and median
	local meanlwidth 0.25
	local p50lwidth 0.25
	

	* The color for text displayed on the graph for mean etc.
	local meanintensity 1.5
	
	* Adding a line for "mean" if required. Change to "Y" if you want to plot the mean, else change to "N"
	local tmean 	"Y"
	
	
	* Adding a line for "median" if required. Change to "Y" if you want to plot the median, else change to "N"
	local tmedian_line 	"N"	
	
	
	* Adding a marker for "median" if required. Change to "Y" if you want to plot the median, else change to "N"
	local tmedian 	"Y"	
	
	
	* Adding a marker for area under the line
	local area_under "Y"
	
	if "`area_under'" == "Y" {
		
		local txtc = 4
		
	}
	
	
	* Loop to generate plotting commands for generating the area plots for each topic
	local mc = 1 		// Mean Color number, to follow what is being called in colorpalette
	local c = 0 		// Number being used to center align topic text within each area chart
	levelsof topic, local(topics)
	foreach topic of local topics {
		
		display "`topic'"
		
		preserve
				
			keep if topic == "`topic'"
			
			drop x_axis
			
			generate x_axis = _n + `offset' - 1
			
			quietly: summarize Overall, detail
			local maxy  	= r(max)
			local miny  	= r(min)
			local meany 	= r(mean)
			local mediany 	= r(p50)
			
			summarize x_axis, meanonly
			local xmin = r(min)
			local xmax = r(max)
			
			summarize hundred, meanonly
			local maxall = r(max)
			
			* Area under the line
			capture integ Overall x_axis, trapezoid
			local raw_area = r(integral)
			
			capture integ hundred x_axis, trapezoid
			local raw_area_all = r(integral)
			
			local area_under_line : display `=(`raw_area'/`raw_area_all') * 100'	
			
			if "`area_under'" == "Y" {
				
				colorpalette tableau, nograph
				local areaunder "`areaunder' (scatteri `txtc' `=`center' + ((`economies' + `jump') * `c')' "Area: `: display %2.1f `area_under_line''%", ms(i) mlabpos(0) mlabsize(*0.6) mlabcolor("`r(p`mc')'*`meanintensity'") mlabgap(*-0.2))"
			
			}
					
					
			* Coding the plot 
			forvalues i = 1/`=_N' {
				
				* Coding all the economies in each row of the topic
				local area "`area' `=Overall[`i']' `=x_axis[`i']' "
			
			}
			
			* This codes the upper portion of the plot which shows how far each economy is from
			* achieving 100% by topic
			local `topic' "(scatteri `maxy' `xmax' `maxall' `xmax' `maxall' `xmin' `miny' `xmin' `area', recast(area) fcolor(gs5%`g_opacity') lwidth(0.15) lcolor(black%50) lalign(inside) nodropbase)"
			
			* This part gathers all topics in a loop and creates a list for aspiration
			local aspire "`aspire' ``topic''"
			
			* This part determines the mean and plots it for the topic
			if "`tmean'" == "Y" {
				
				colorpalette tableau, nograph				
				local taverage "`taverage' (scatteri `meany' `xmin' `meany' `xmax', recast(line) lcolor("`r(p`mc')'*`meanintensity'") lwidth(`meanlwidth'))"
				
				* Positioning of "mean" label in case mean is greater than median and vice versa
				if (`meany' > `mediany') {
					
					local taveragel "`taveragel' (scatteri `meany' `=`center' + ((`economies' + `jump') * `c')' "Mean: `: display %2.1f `meany''%", ms(i) mlabpos(12) mlabsize(*0.5) mlabcolor("`r(p`mc')'*`meanintensity'") mlabgap(*-0.2))"
					
				}
				
				else {
					
					local taveragel "`taveragel' (scatteri `meany' `=`center' + ((`economies' + `jump') * `c')' "Mean: `: display %2.1f `meany''%", ms(i) mlabpos(6) mlabsize(*0.5) mlabcolor("`r(p`mc')'*`meanintensity'") mlabgap(*-1))"
							
				}				
						
			}
			
			
			* This part determines the median and plots it for the topic
			* If median line in the local is enabled then the following code will execute			
			if "`tmedian_line'" == "Y" {
				
				local tp50line "`tp50line' (scatteri `mediany' `xmin' `mediany' `xmax', recast(line) lcolor(black) lwidth(`p50lwidth'))"
				
				* Positioning of "median" label in case median is greater than mean and vice versa
				if (`mediany' > `meany') {
					
					local tp50linel "`tp50linel' (scatteri `mediany' `=`center' + ((`economies' + `jump') * `c')' "Median: `: display %2.1f `mediany''%", ms(i) mlabpos(12) mlabsize(*0.5) mlabcolor(black) mlabgap(*-0.2))"
					
				}
				
				else {
					
					local tp50linel "`tp50linel' (scatteri `mediany' `=`center' + ((`economies' + `jump') * `c')' "Median: `: display %2.1f `mediany''%", ms(i) mlabpos(6) mlabsize(*0.5) mlabcolor(black) mlabgap(*-1))"
							
				}				
					
			}
			
			* If median marker in the local is enabled then the following code will execute
			if "`tmedian'" == "Y" {
				
				local tp50 "`tp50' (scatteri `mediany' `=`center' + ((`economies' + `jump') * `c')', mlcolor(black) msize(*0.5) msymbol(Oh) mlwidth(*0.5))"
				
				* Positioning of "median" label in case median is greater than mean and vice versa
				if (`mediany' > `meany') {
					
					local tp50l "`tp50l' (scatteri `mediany' `=`center' + ((`economies' + `jump') * `c')' "Median: `: display %2.1f `mediany''%", ms(i) mlabpos(12) mlabsize(*0.5) mlabcolor(black) mlabgap(*0.4))"
					
				}
				
				else {
					
					local tp50l "`tp50l' (scatteri `mediany' `=`center' + ((`economies' + `jump') * `c')' "Median: `: display %2.1f `mediany''%", ms(i) mlabpos(6) mlabsize(*0.5) mlabcolor(black) mlabgap(*-1))"
							
				}				
					
			}			
			
			
			* Clearing out the local area to start fresh in the loop
			local area 
			
			* Move topic counter to the next topic
			local ++c
			
			* Move to the next label color to match topic
			local ++mc
			
		restore
		
		* The following compound loop below codes the scores that each economy
		* achieved by topic (as a single color or colored by topic)
		* local achieved "`achieved' (area Overall x_axis if topic == "`topic'", base(0) color("255 127 14%15") lwidth(0.1)) "
		colorpalette tableau, nograph
		*color_style vangogh2, ipolate(10)
		local achieved "`achieved' (area Overall x_axis if topic == "`topic'", fcolor("`r(p`j')'%`c_opacity'") lwidth(0) base(0)) "
		
		local offset = `offset' + `economies' + `jump'
		local ++j
		
	}	

	* display "`aspire'"
	
	* display `"`achieved'"'
	
	
	* Plotting the code generated in the above loops including labels for each topic
	twoway 		///
	`achieved' 	/// Area plot for scores achieved
	`aspire' 	/// Area plot for gap between achieved and 100%
	`taverage' 	/// Mean
	`taveragel' /// Mean Labels
	`tp50line' 	/// Median Line 
	`tp50linel' /// Median line Labels
	`tp50' 		/// Median Marker
	`tp50l' 	/// Median Marker Labels
	`areaunder' /// Display area under line
	(scatteri `=`economies' + 4' `=`economies' + 20' "Although some economies achieve high scores, but overall,", ms(i) mlabsize(*0.5) mlabcolor(gs3)) ///
	(scatteri `=`economies' + 2' `=`economies' + 20' "there is large room for improvement within each group", ms(i) mlabsize(*0.5) mlabcolor(gs3)) ///	
	(scatteri `=`economies' + 3' `economies' `=`economies' + 3' `=`economies' + 20', recast(line) lcolor(black) lwidth(0.1)) ///
	(scatteri `=`economies' - 5' `center' `=`economies' + 3' `economies', recast(line) lcolor(black) lwidth(0.1)) ///
	(scatteri `=`economies' - 5' `center', mfcolor(white) mlwidth(0.1) mlcolor(black) msize(1.3)) ///
	(scatteri `=`economies' - 5' `center', mfcolor(black) mlwidth(0.1) mlcolor(black) msize(0.3)) ///
	(scatteri 0 0 "0%", ms(i) mlabpos(9) mlabcolor(gs3) mlabsize(*0.65)) ///
	(scatteri `economies' 0 "100%", ms(i) mlabpos(9) mlabcolor(gs3) mlabsize(*0.65)) ///
	(scatteri `txta' `=`center' + ((`economies' + `jump') * 0)' "{bf}Group", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txtb' `=`center' + ((`economies' + `jump') * 0)' "{bf}A", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txta' `=`center' + ((`economies' + `jump') * 1)' "{bf}Group", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txtb' `=`center' + ((`economies' + `jump') * 1)' "{bf}B", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txta' `=`center' + ((`economies' + `jump') * 2)' "{bf}Group", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txtb' `=`center' + ((`economies' + `jump') * 2)' "{bf}C", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `=(`txta' + `txtb')/2' `=`center' + ((`economies' + `jump') * 3)' "{bf}Group D", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txta' `=`center' + ((`economies' + `jump') * 4)' "{bf}Group", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txtb' `=`center' + ((`economies' + `jump') * 4)' "{bf}E", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txta' `=`center' + ((`economies' + `jump') * 5)' "{bf}Group", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txtb' `=`center' + ((`economies' + `jump') * 5)' "{bf}F", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `=(`txta' + `txtb')/2' `=`center' + ((`economies' + `jump') * 6)' "{bf}Group G", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txta' `=`center' + ((`economies' + `jump') * 7)' "{bf}Group", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txtb' `=`center' + ((`economies' + `jump') * 7)' "{bf}H", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///	
	(scatteri `txta' `=`center' + ((`economies' + `jump') * 8)' "{bf}Group", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txtb' `=`center' + ((`economies' + `jump') * 8)' "{bf}I", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txta' `=`center' + ((`economies' + `jump') * 9)' "{bf}Group", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///
	(scatteri `txtb' `=`center' + ((`economies' + `jump') * 9)' "{bf}J", ms(i) mlabpos(0) mlabsize(*`topic_size') mlabcolor(gs6)) ///		
	, ///
	ylabel(none, nogrid labsize(*0.5) noticks) ///
	ytitle("{bf}Points achieved for each economy", size(*0.7) color(gs6)) ///
	yscale(noline titlegap(-0.75)) ///
	xlabel(none, nogrid) ///
	xtitle("{bf}`economies' economies per group", size(*.7) color(gs6)) ///
	xscale(noline titlegap(-0.75)) ///
	graphregion(margin(l = 1.2 r = 0 t = -3 b = -3)) ///
	legend(off)
	
	
	* Exporting the plot in both PNG and EPS formats
	graph export "./Chart_Multi_Panel_Area_Plot.png", as(png) width(3840) replace	
	* graph export "./Chart_Multi_Panel_Area_Plot.eps", as(eps) replace		