#!/usr/bin/env perl

# read in output of lepto simulation, recast it in a form
# suitable for txt2part

$beam_energy = 5.7;

$oldrun = 0;
$event = 0;
$skip = 1; 

$file_line_number = 0;

($z_shift)=@ARGV;

store_pdg();

while (<STDIN>) { # read in a line from stdin 
	chomp;
	@field=split(/ +/); # split line on spaces
	$file_line_number++;
#			printf "%8.3f %8.3f %8.3f %8.3f %8.3f \n", $field[1], $field[2], $field[3], $field[4], $field[5];


	
	if (($field[1] eq "Event") && ($field[2] eq "listing") ) { 
		$skip=0; 
	} # skip top part of file completely
	
	if($skip == 0){
		if ($field[1] ne "1") { # still on same run
			if( $field[3] eq "1" && $field[1] ne "N" ) { # final particle
				$num++;
				fill();
			}
		}
#and $field[1] eq "!e-!"
		if ($field[1] eq "1" ) { # on new run
			if($event > 1){
			writeout(); # write out results of previous event
		}
			$num=0;
			$event=$event+1;

		}			
	}
} # end of main loop

########################################################
sub fill() {
	
# fill the arrays
	
	$geant_id = 0;
	
	get_geant(); 
	
# writing this out confuses txt2part!
#	if($geant_id == 0){
#		print "Particle $field[4] not found";
#	}
	
	$x[$num]       = 0.;
	$y[$num]       = 0.;
	$z[$num]       = 0.-$z_shift;
	$px[$num]      = $field[6];
	$py[$num]      = $field[7];
	$pz[$num]      = $field[8];
	$E[$num]       = $field[9];
	$idpart[$num]  = $geant_id;
}

#########################################################
sub writeout() {
	
# num is number of lines per event
	
	$i=1;
	if($num > 0) {
		print "$num\n";
		while ($i <= $num) {
			printf "%2d %9.7f %9.7f %9.7f %9.7f\n", $idpart[$i], $E[$i], $px[$i], $py[$i], $pz[$i];
			printf "%8.3f %8.3f %8.3f \n", $x[$i], $y[$i], $z[$i];
			$i++;
		}
	}
}

##########################################################
sub get_geant() {

	$j=1;
	while ($j < 100){
		if ($field[4] == $pdg[$j]) {$geant_id = $j;}
		$j++;
	}
} 


##########################################################
sub store_pdg() {

# adapted from eliott's code in clas_utils, which he stole from cernlib

$pdg[1]  = 22;   
$pdg[2]  = -11;
$pdg[3]  = 11;    
$pdg[4]  = 12;   
$pdg[5]  = -13;    
$pdg[6]  = 13;   
$pdg[7]  = 111;   
$pdg[8]  = 211;
$pdg[9]  = -211;   
$pdg[10] = 130;   
$pdg[11] = 321;  
$pdg[12] = -321;  
$pdg[13] = 2112;  
$pdg[14] = 2212; 
$pdg[15] = -2212;   
$pdg[16] = 310;
$pdg[17] = 221;  
$pdg[18] = 3122;  
$pdg[19] = 3222;  
$pdg[20] = 3212;  
$pdg[21] = 3112;  
$pdg[22] = 3322;  
$pdg[23] = 3312;  
$pdg[24] = 3334;
$pdg[25] = -2112; 
$pdg[26] = -3122;
$pdg[27] = -3112; 
$pdg[28] = -3212; 
$pdg[29] = -3222; 
$pdg[30] = -3322; 
$pdg[31] = -3312; 
$pdg[32] = -3334;
$pdg[33] = -15;    
$pdg[34] = 15;   
$pdg[35] = 411;  
$pdg[36] = -411;   
$pdg[37] = 421;  
$pdg[38] = -421;   
$pdg[39] = 431;  
$pdg[40] = -431;
$pdg[41] = 4122;   
$pdg[42] = 24;   
$pdg[43] = -24;    
$pdg[44] = 23;

}









