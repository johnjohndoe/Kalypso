// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/io/geotiff/Projected_CS_Codes.java,v
// 1.1 2004/08/18 08:50:17 axel_schaefer Exp $
/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de


 ---------------------------------------------------------------------------*/
package org.deegree_impl.io.geotiff;

import java.util.HashMap;

/**
 * Projected CS Codes
 * 
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </A>
 * @author last edited by: $Author$
 * @version 2.0. $Revision$, $Date$
 * @since 2.0
 */
public class Projected_CS_Codes
{

  private static HashMap projected_cs_type_codes = null;

  /**
   * private constructor. static initializer
   */
  private Projected_CS_Codes()
  {}

  static
  {
    projected_cs_type_codes = new HashMap( 500 );

    projected_cs_type_codes.put( new Integer( 20137 ), "PCS_Adindan_UTM_zone_37N" );
    projected_cs_type_codes.put( new Integer( 20138 ), "PCS_Adindan_UTM_zone_38N" );
    projected_cs_type_codes.put( new Integer( 20248 ), "PCS_AGD66_AMG_zone_48" );
    projected_cs_type_codes.put( new Integer( 20249 ), "PCS_AGD66_AMG_zone_49" );
    projected_cs_type_codes.put( new Integer( 20250 ), "PCS_AGD66_AMG_zone_50" );
    projected_cs_type_codes.put( new Integer( 20251 ), "PCS_AGD66_AMG_zone_51" );
    projected_cs_type_codes.put( new Integer( 20252 ), "PCS_AGD66_AMG_zone_52" );
    projected_cs_type_codes.put( new Integer( 20253 ), "PCS_AGD66_AMG_zone_53" );
    projected_cs_type_codes.put( new Integer( 20254 ), "PCS_AGD66_AMG_zone_54" );
    projected_cs_type_codes.put( new Integer( 20255 ), "PCS_AGD66_AMG_zone_55" );
    projected_cs_type_codes.put( new Integer( 20256 ), "PCS_AGD66_AMG_zone_56" );
    projected_cs_type_codes.put( new Integer( 20257 ), "PCS_AGD66_AMG_zone_57" );
    projected_cs_type_codes.put( new Integer( 20258 ), "PCS_AGD66_AMG_zone_58" );
    projected_cs_type_codes.put( new Integer( 20348 ), "PCS_AGD84_AMG_zone_48" );
    projected_cs_type_codes.put( new Integer( 20349 ), "PCS_AGD84_AMG_zone_49" );
    projected_cs_type_codes.put( new Integer( 20350 ), "PCS_AGD84_AMG_zone_50" );
    projected_cs_type_codes.put( new Integer( 20351 ), "PCS_AGD84_AMG_zone_51" );
    projected_cs_type_codes.put( new Integer( 20352 ), "PCS_AGD84_AMG_zone_52" );
    projected_cs_type_codes.put( new Integer( 20353 ), "PCS_AGD84_AMG_zone_53" );
    projected_cs_type_codes.put( new Integer( 20354 ), "PCS_AGD84_AMG_zone_54" );
    projected_cs_type_codes.put( new Integer( 20355 ), "PCS_AGD84_AMG_zone_55" );
    projected_cs_type_codes.put( new Integer( 20356 ), "PCS_AGD84_AMG_zone_56" );
    projected_cs_type_codes.put( new Integer( 20357 ), "PCS_AGD84_AMG_zone_57" );
    projected_cs_type_codes.put( new Integer( 20358 ), "PCS_AGD84_AMG_zone_58" );
    projected_cs_type_codes.put( new Integer( 20437 ), "PCS_Ain_el_Abd_UTM_zone_37N" );
    projected_cs_type_codes.put( new Integer( 20438 ), "PCS_Ain_el_Abd_UTM_zone_38N" );
    projected_cs_type_codes.put( new Integer( 20439 ), "PCS_Ain_el_Abd_UTM_zone_39N" );
    projected_cs_type_codes.put( new Integer( 20499 ), "PCS_Ain_el_Abd_Bahrain_Grid" );
    projected_cs_type_codes.put( new Integer( 20538 ), "PCS_Afgooye_UTM_zone_38N" );
    projected_cs_type_codes.put( new Integer( 20539 ), "PCS_Afgooye_UTM_zone_39N" );
    projected_cs_type_codes.put( new Integer( 20700 ), "PCS_Lisbon_Portugese_Grid" );
    projected_cs_type_codes.put( new Integer( 20822 ), "PCS_Aratu_UTM_zone_22S" );
    projected_cs_type_codes.put( new Integer( 20823 ), "PCS_Aratu_UTM_zone_23S" );
    projected_cs_type_codes.put( new Integer( 20824 ), "PCS_Aratu_UTM_zone_24S" );
    projected_cs_type_codes.put( new Integer( 20973 ), "PCS_Arc_1950_Lo13" );
    projected_cs_type_codes.put( new Integer( 20975 ), "PCS_Arc_1950_Lo15" );
    projected_cs_type_codes.put( new Integer( 20977 ), "PCS_Arc_1950_Lo17" );
    projected_cs_type_codes.put( new Integer( 20979 ), "PCS_Arc_1950_Lo19" );
    projected_cs_type_codes.put( new Integer( 20981 ), "PCS_Arc_1950_Lo21" );
    projected_cs_type_codes.put( new Integer( 20983 ), "PCS_Arc_1950_Lo23" );
    projected_cs_type_codes.put( new Integer( 20985 ), "PCS_Arc_1950_Lo25" );
    projected_cs_type_codes.put( new Integer( 20987 ), "PCS_Arc_1950_Lo27" );
    projected_cs_type_codes.put( new Integer( 20989 ), "PCS_Arc_1950_Lo29" );
    projected_cs_type_codes.put( new Integer( 20991 ), "PCS_Arc_1950_Lo31" );
    projected_cs_type_codes.put( new Integer( 20993 ), "PCS_Arc_1950_Lo33" );
    projected_cs_type_codes.put( new Integer( 20995 ), "PCS_Arc_1950_Lo35" );
    projected_cs_type_codes.put( new Integer( 21100 ), "PCS_Batavia_NEIEZ" );
    projected_cs_type_codes.put( new Integer( 21148 ), "PCS_Batavia_UTM_zone_48S" );
    projected_cs_type_codes.put( new Integer( 21149 ), "PCS_Batavia_UTM_zone_49S" );
    projected_cs_type_codes.put( new Integer( 21150 ), "PCS_Batavia_UTM_zone_50S" );
    projected_cs_type_codes.put( new Integer( 21413 ), "PCS_Beijing_Gauss_zone_13" );
    projected_cs_type_codes.put( new Integer( 21414 ), "PCS_Beijing_Gauss_zone_14" );
    projected_cs_type_codes.put( new Integer( 21415 ), "PCS_Beijing_Gauss_zone_15" );
    projected_cs_type_codes.put( new Integer( 21416 ), "PCS_Beijing_Gauss_zone_16" );
    projected_cs_type_codes.put( new Integer( 21417 ), "PCS_Beijing_Gauss_zone_17" );
    projected_cs_type_codes.put( new Integer( 21418 ), "PCS_Beijing_Gauss_zone_18" );
    projected_cs_type_codes.put( new Integer( 21419 ), "PCS_Beijing_Gauss_zone_19" );
    projected_cs_type_codes.put( new Integer( 21420 ), "PCS_Beijing_Gauss_zone_20" );
    projected_cs_type_codes.put( new Integer( 21421 ), "PCS_Beijing_Gauss_zone_21" );
    projected_cs_type_codes.put( new Integer( 21422 ), "PCS_Beijing_Gauss_zone_22" );
    projected_cs_type_codes.put( new Integer( 21423 ), "PCS_Beijing_Gauss_zone_23" );
    projected_cs_type_codes.put( new Integer( 21473 ), "PCS_Beijing_Gauss_13N" );
    projected_cs_type_codes.put( new Integer( 21474 ), "PCS_Beijing_Gauss_14N" );
    projected_cs_type_codes.put( new Integer( 21475 ), "PCS_Beijing_Gauss_15N" );
    projected_cs_type_codes.put( new Integer( 21476 ), "PCS_Beijing_Gauss_16N" );
    projected_cs_type_codes.put( new Integer( 21477 ), "PCS_Beijing_Gauss_17N" );
    projected_cs_type_codes.put( new Integer( 21478 ), "PCS_Beijing_Gauss_18N" );
    projected_cs_type_codes.put( new Integer( 21479 ), "PCS_Beijing_Gauss_19N" );
    projected_cs_type_codes.put( new Integer( 21480 ), "PCS_Beijing_Gauss_20N" );
    projected_cs_type_codes.put( new Integer( 21481 ), "PCS_Beijing_Gauss_21N" );
    projected_cs_type_codes.put( new Integer( 21482 ), "PCS_Beijing_Gauss_22N" );
    projected_cs_type_codes.put( new Integer( 21483 ), "PCS_Beijing_Gauss_23N" );
    projected_cs_type_codes.put( new Integer( 21500 ), "PCS_Belge_Lambert_50" );
    projected_cs_type_codes.put( new Integer( 21790 ), "PCS_Bern_1898_Swiss_Old" );
    projected_cs_type_codes.put( new Integer( 21817 ), "PCS_Bogota_UTM_zone_17N" );
    projected_cs_type_codes.put( new Integer( 21818 ), "PCS_Bogota_UTM_zone_18N" );
    projected_cs_type_codes.put( new Integer( 21891 ), "PCS_Bogota_Colombia_3W" );
    projected_cs_type_codes.put( new Integer( 21892 ), "PCS_Bogota_Colombia_Bogota" );
    projected_cs_type_codes.put( new Integer( 21893 ), "PCS_Bogota_Colombia_3E" );
    projected_cs_type_codes.put( new Integer( 21894 ), "PCS_Bogota_Colombia_6E" );
    projected_cs_type_codes.put( new Integer( 22032 ), "PCS_Camacupa_UTM_32S" );
    projected_cs_type_codes.put( new Integer( 22033 ), "PCS_Camacupa_UTM_33S" );
    projected_cs_type_codes.put( new Integer( 22191 ), "PCS_C_Inchauspe_Argentina_1" );
    projected_cs_type_codes.put( new Integer( 22192 ), "PCS_C_Inchauspe_Argentina_2" );
    projected_cs_type_codes.put( new Integer( 22193 ), "PCS_C_Inchauspe_Argentina_3" );
    projected_cs_type_codes.put( new Integer( 22194 ), "PCS_C_Inchauspe_Argentina_4" );
    projected_cs_type_codes.put( new Integer( 22195 ), "PCS_C_Inchauspe_Argentina_5" );
    projected_cs_type_codes.put( new Integer( 22196 ), "PCS_C_Inchauspe_Argentina_6" );
    projected_cs_type_codes.put( new Integer( 22197 ), "PCS_C_Inchauspe_Argentina_7" );
    projected_cs_type_codes.put( new Integer( 22332 ), "PCS_Carthage_UTM_zone_32N" );
    projected_cs_type_codes.put( new Integer( 22391 ), "PCS_Carthage_Nord_Tunisie" );
    projected_cs_type_codes.put( new Integer( 22392 ), "PCS_Carthage_Sud_Tunisie" );
    projected_cs_type_codes.put( new Integer( 22523 ), "PCS_Corrego_Alegre_UTM_23S" );
    projected_cs_type_codes.put( new Integer( 22524 ), "PCS_Corrego_Alegre_UTM_24S" );
    projected_cs_type_codes.put( new Integer( 22832 ), "PCS_Douala_UTM_zone_32N" );
    projected_cs_type_codes.put( new Integer( 22992 ), "PCS_Egypt_1907_Red_Belt" );
    projected_cs_type_codes.put( new Integer( 22993 ), "PCS_Egypt_1907_Purple_Belt" );
    projected_cs_type_codes.put( new Integer( 22994 ), "PCS_Egypt_1907_Ext_Purple" );
    projected_cs_type_codes.put( new Integer( 23028 ), "PCS_ED50_UTM_zone_28N" );
    projected_cs_type_codes.put( new Integer( 23029 ), "PCS_ED50_UTM_zone_29N" );
    projected_cs_type_codes.put( new Integer( 23030 ), "PCS_ED50_UTM_zone_30N" );
    projected_cs_type_codes.put( new Integer( 23031 ), "PCS_ED50_UTM_zone_31N" );
    projected_cs_type_codes.put( new Integer( 23032 ), "PCS_ED50_UTM_zone_32N" );
    projected_cs_type_codes.put( new Integer( 23033 ), "PCS_ED50_UTM_zone_33N" );
    projected_cs_type_codes.put( new Integer( 23034 ), "PCS_ED50_UTM_zone_34N" );
    projected_cs_type_codes.put( new Integer( 23035 ), "PCS_ED50_UTM_zone_35N" );
    projected_cs_type_codes.put( new Integer( 23036 ), "PCS_ED50_UTM_zone_36N" );
    projected_cs_type_codes.put( new Integer( 23037 ), "PCS_ED50_UTM_zone_37N" );
    projected_cs_type_codes.put( new Integer( 23038 ), "PCS_ED50_UTM_zone_38N" );
    projected_cs_type_codes.put( new Integer( 23239 ), "PCS_Fahud_UTM_zone_39N" );
    projected_cs_type_codes.put( new Integer( 23240 ), "PCS_Fahud_UTM_zone_40N" );
    projected_cs_type_codes.put( new Integer( 23433 ), "PCS_Garoua_UTM_zone_33N" );
    projected_cs_type_codes.put( new Integer( 23846 ), "PCS_ID74_UTM_zone_46N" );
    projected_cs_type_codes.put( new Integer( 23847 ), "PCS_ID74_UTM_zone_47N" );
    projected_cs_type_codes.put( new Integer( 23848 ), "PCS_ID74_UTM_zone_48N" );
    projected_cs_type_codes.put( new Integer( 23849 ), "PCS_ID74_UTM_zone_49N" );
    projected_cs_type_codes.put( new Integer( 23850 ), "PCS_ID74_UTM_zone_50N" );
    projected_cs_type_codes.put( new Integer( 23851 ), "PCS_ID74_UTM_zone_51N" );
    projected_cs_type_codes.put( new Integer( 23852 ), "PCS_ID74_UTM_zone_52N" );
    projected_cs_type_codes.put( new Integer( 23853 ), "PCS_ID74_UTM_zone_53N" );
    projected_cs_type_codes.put( new Integer( 23886 ), "PCS_ID74_UTM_zone_46S" );
    projected_cs_type_codes.put( new Integer( 23887 ), "PCS_ID74_UTM_zone_47S" );
    projected_cs_type_codes.put( new Integer( 23888 ), "PCS_ID74_UTM_zone_48S" );
    projected_cs_type_codes.put( new Integer( 23889 ), "PCS_ID74_UTM_zone_49S" );
    projected_cs_type_codes.put( new Integer( 23890 ), "PCS_ID74_UTM_zone_50S" );
    projected_cs_type_codes.put( new Integer( 23891 ), "PCS_ID74_UTM_zone_51S" );
    projected_cs_type_codes.put( new Integer( 23892 ), "PCS_ID74_UTM_zone_52S" );
    projected_cs_type_codes.put( new Integer( 23893 ), "PCS_ID74_UTM_zone_53S" );
    projected_cs_type_codes.put( new Integer( 23894 ), "PCS_ID74_UTM_zone_54S" );
    projected_cs_type_codes.put( new Integer( 23947 ), "PCS_Indian_1954_UTM_47N" );
    projected_cs_type_codes.put( new Integer( 23948 ), "PCS_Indian_1954_UTM_48N" );
    projected_cs_type_codes.put( new Integer( 24047 ), "PCS_Indian_1975_UTM_47N" );
    projected_cs_type_codes.put( new Integer( 24048 ), "PCS_Indian_1975_UTM_48N" );
    projected_cs_type_codes.put( new Integer( 24100 ), "PCS_Jamaica_1875_Old_Grid" );
    projected_cs_type_codes.put( new Integer( 24200 ), "PCS_JAD69_Jamaica_Grid" );
    projected_cs_type_codes.put( new Integer( 24370 ), "PCS_Kalianpur_India_0" );
    projected_cs_type_codes.put( new Integer( 24371 ), "PCS_Kalianpur_India_I" );
    projected_cs_type_codes.put( new Integer( 24372 ), "PCS_Kalianpur_India_IIa" );
    projected_cs_type_codes.put( new Integer( 24373 ), "PCS_Kalianpur_India_IIIa" );
    projected_cs_type_codes.put( new Integer( 24374 ), "PCS_Kalianpur_India_IVa" );
    projected_cs_type_codes.put( new Integer( 24382 ), "PCS_Kalianpur_India_IIb" );
    projected_cs_type_codes.put( new Integer( 24383 ), "PCS_Kalianpur_India_IIIb" );
    projected_cs_type_codes.put( new Integer( 24384 ), "PCS_Kalianpur_India_IVb" );
    projected_cs_type_codes.put( new Integer( 24500 ), "PCS_Kertau_Singapore_Grid" );
    projected_cs_type_codes.put( new Integer( 24547 ), "PCS_Kertau_UTM_zone_47N" );
    projected_cs_type_codes.put( new Integer( 24548 ), "PCS_Kertau_UTM_zone_48N" );
    projected_cs_type_codes.put( new Integer( 24720 ), "PCS_La_Canoa_UTM_zone_20N" );
    projected_cs_type_codes.put( new Integer( 24721 ), "PCS_La_Canoa_UTM_zone_21N" );
    projected_cs_type_codes.put( new Integer( 24818 ), "PCS_PSAD56_UTM_zone_18N" );
    projected_cs_type_codes.put( new Integer( 24819 ), "PCS_PSAD56_UTM_zone_19N" );
    projected_cs_type_codes.put( new Integer( 24820 ), "PCS_PSAD56_UTM_zone_20N" );
    projected_cs_type_codes.put( new Integer( 24821 ), "PCS_PSAD56_UTM_zone_21N" );
    projected_cs_type_codes.put( new Integer( 24877 ), "PCS_PSAD56_UTM_zone_17S" );
    projected_cs_type_codes.put( new Integer( 24878 ), "PCS_PSAD56_UTM_zone_18S" );
    projected_cs_type_codes.put( new Integer( 24879 ), "PCS_PSAD56_UTM_zone_19S" );
    projected_cs_type_codes.put( new Integer( 24880 ), "PCS_PSAD56_UTM_zone_20S" );
    projected_cs_type_codes.put( new Integer( 24891 ), "PCS_PSAD56_Peru_west_zone" );
    projected_cs_type_codes.put( new Integer( 24892 ), "PCS_PSAD56_Peru_central" );
    projected_cs_type_codes.put( new Integer( 24893 ), "PCS_PSAD56_Peru_east_zone" );
    projected_cs_type_codes.put( new Integer( 25000 ), "PCS_Leigon_Ghana_Grid" );
    projected_cs_type_codes.put( new Integer( 25231 ), "PCS_Lome_UTM_zone_31N" );
    projected_cs_type_codes.put( new Integer( 25391 ), "PCS_Luzon_Philippines_I" );
    projected_cs_type_codes.put( new Integer( 25392 ), "PCS_Luzon_Philippines_II" );
    projected_cs_type_codes.put( new Integer( 25393 ), "PCS_Luzon_Philippines_III" );
    projected_cs_type_codes.put( new Integer( 25394 ), "PCS_Luzon_Philippines_IV" );
    projected_cs_type_codes.put( new Integer( 25395 ), "PCS_Luzon_Philippines_V" );
    projected_cs_type_codes.put( new Integer( 25700 ), "PCS_Makassar_NEIEZ" );
    projected_cs_type_codes.put( new Integer( 25932 ), "PCS_Malongo_1987_UTM_32S" );
    projected_cs_type_codes.put( new Integer( 26191 ), "PCS_Merchich_Nord_Maroc" );
    projected_cs_type_codes.put( new Integer( 26192 ), "PCS_Merchich_Sud_Maroc" );
    projected_cs_type_codes.put( new Integer( 26193 ), "PCS_Merchich_Sahara" );
    projected_cs_type_codes.put( new Integer( 26237 ), "PCS_Massawa_UTM_zone_37N" );
    projected_cs_type_codes.put( new Integer( 26331 ), "PCS_Minna_UTM_zone_31N" );
    projected_cs_type_codes.put( new Integer( 26332 ), "PCS_Minna_UTM_zone_32N" );
    projected_cs_type_codes.put( new Integer( 26391 ), "PCS_Minna_Nigeria_West" );
    projected_cs_type_codes.put( new Integer( 26392 ), "PCS_Minna_Nigeria_Mid_Belt" );
    projected_cs_type_codes.put( new Integer( 26393 ), "PCS_Minna_Nigeria_East" );
    projected_cs_type_codes.put( new Integer( 26432 ), "PCS_Mhast_UTM_zone_32S" );
    projected_cs_type_codes.put( new Integer( 26591 ), "PCS_Monte_Mario_Italy_1" );
    projected_cs_type_codes.put( new Integer( 26592 ), "PCS_Monte_Mario_Italy_2" );
    projected_cs_type_codes.put( new Integer( 26632 ), "PCS_M_poraloko_UTM_32N" );
    projected_cs_type_codes.put( new Integer( 26692 ), "PCS_M_poraloko_UTM_32S" );
    projected_cs_type_codes.put( new Integer( 26703 ), "PCS_NAD27_UTM_zone_3N" );
    projected_cs_type_codes.put( new Integer( 26704 ), "PCS_NAD27_UTM_zone_4N" );
    projected_cs_type_codes.put( new Integer( 26705 ), "PCS_NAD27_UTM_zone_5N" );
    projected_cs_type_codes.put( new Integer( 26706 ), "PCS_NAD27_UTM_zone_6N" );
    projected_cs_type_codes.put( new Integer( 26707 ), "PCS_NAD27_UTM_zone_7N" );
    projected_cs_type_codes.put( new Integer( 26708 ), "PCS_NAD27_UTM_zone_8N" );
    projected_cs_type_codes.put( new Integer( 26709 ), "PCS_NAD27_UTM_zone_9N" );
    projected_cs_type_codes.put( new Integer( 26710 ), "PCS_NAD27_UTM_zone_10N" );
    projected_cs_type_codes.put( new Integer( 26711 ), "PCS_NAD27_UTM_zone_11N" );
    projected_cs_type_codes.put( new Integer( 26712 ), "PCS_NAD27_UTM_zone_12N" );
    projected_cs_type_codes.put( new Integer( 26713 ), "PCS_NAD27_UTM_zone_13N" );
    projected_cs_type_codes.put( new Integer( 26714 ), "PCS_NAD27_UTM_zone_14N" );
    projected_cs_type_codes.put( new Integer( 26715 ), "PCS_NAD27_UTM_zone_15N" );
    projected_cs_type_codes.put( new Integer( 26716 ), "PCS_NAD27_UTM_zone_16N" );
    projected_cs_type_codes.put( new Integer( 26717 ), "PCS_NAD27_UTM_zone_17N" );
    projected_cs_type_codes.put( new Integer( 26718 ), "PCS_NAD27_UTM_zone_18N" );
    projected_cs_type_codes.put( new Integer( 26719 ), "PCS_NAD27_UTM_zone_19N" );
    projected_cs_type_codes.put( new Integer( 26720 ), "PCS_NAD27_UTM_zone_20N" );
    projected_cs_type_codes.put( new Integer( 26721 ), "PCS_NAD27_UTM_zone_21N" );
    projected_cs_type_codes.put( new Integer( 26722 ), "PCS_NAD27_UTM_zone_22N" );
    projected_cs_type_codes.put( new Integer( 26729 ), "PCS_NAD27_Alabama_East" );
    projected_cs_type_codes.put( new Integer( 26730 ), "PCS_NAD27_Alabama_West" );
    projected_cs_type_codes.put( new Integer( 26731 ), "PCS_NAD27_Alaska_zone_1" );
    projected_cs_type_codes.put( new Integer( 26732 ), "PCS_NAD27_Alaska_zone_2" );
    projected_cs_type_codes.put( new Integer( 26733 ), "PCS_NAD27_Alaska_zone_3" );
    projected_cs_type_codes.put( new Integer( 26734 ), "PCS_NAD27_Alaska_zone_4" );
    projected_cs_type_codes.put( new Integer( 26735 ), "PCS_NAD27_Alaska_zone_5" );
    projected_cs_type_codes.put( new Integer( 26736 ), "PCS_NAD27_Alaska_zone_6" );
    projected_cs_type_codes.put( new Integer( 26737 ), "PCS_NAD27_Alaska_zone_7" );
    projected_cs_type_codes.put( new Integer( 26738 ), "PCS_NAD27_Alaska_zone_8" );
    projected_cs_type_codes.put( new Integer( 26739 ), "PCS_NAD27_Alaska_zone_9" );
    projected_cs_type_codes.put( new Integer( 26740 ), "PCS_NAD27_Alaska_zone_10" );
    projected_cs_type_codes.put( new Integer( 26741 ), "PCS_NAD27_California_I" );
    projected_cs_type_codes.put( new Integer( 26742 ), "PCS_NAD27_California_II" );
    projected_cs_type_codes.put( new Integer( 26743 ), "PCS_NAD27_California_III" );
    projected_cs_type_codes.put( new Integer( 26744 ), "PCS_NAD27_California_IV" );
    projected_cs_type_codes.put( new Integer( 26745 ), "PCS_NAD27_California_V" );
    projected_cs_type_codes.put( new Integer( 26746 ), "PCS_NAD27_California_VI" );
    projected_cs_type_codes.put( new Integer( 26747 ), "PCS_NAD27_California_VII" );
    projected_cs_type_codes.put( new Integer( 26748 ), "PCS_NAD27_Arizona_East" );
    projected_cs_type_codes.put( new Integer( 26749 ), "PCS_NAD27_Arizona_Central" );
    projected_cs_type_codes.put( new Integer( 26750 ), "PCS_NAD27_Arizona_West" );
    projected_cs_type_codes.put( new Integer( 26751 ), "PCS_NAD27_Arkansas_North" );
    projected_cs_type_codes.put( new Integer( 26752 ), "PCS_NAD27_Arkansas_South" );
    projected_cs_type_codes.put( new Integer( 26753 ), "PCS_NAD27_Colorado_North" );
    projected_cs_type_codes.put( new Integer( 26754 ), "PCS_NAD27_Colorado_Central" );
    projected_cs_type_codes.put( new Integer( 26755 ), "PCS_NAD27_Colorado_South" );
    projected_cs_type_codes.put( new Integer( 26756 ), "PCS_NAD27_Connecticut" );
    projected_cs_type_codes.put( new Integer( 26757 ), "PCS_NAD27_Delaware" );
    projected_cs_type_codes.put( new Integer( 26758 ), "PCS_NAD27_Florida_East" );
    projected_cs_type_codes.put( new Integer( 26759 ), "PCS_NAD27_Florida_West" );
    projected_cs_type_codes.put( new Integer( 26760 ), "PCS_NAD27_Florida_North" );
    projected_cs_type_codes.put( new Integer( 26761 ), "PCS_NAD27_Hawaii_zone_1" );
    projected_cs_type_codes.put( new Integer( 26762 ), "PCS_NAD27_Hawaii_zone_2" );
    projected_cs_type_codes.put( new Integer( 26763 ), "PCS_NAD27_Hawaii_zone_3" );
    projected_cs_type_codes.put( new Integer( 26764 ), "PCS_NAD27_Hawaii_zone_4" );
    projected_cs_type_codes.put( new Integer( 26765 ), "PCS_NAD27_Hawaii_zone_5" );
    projected_cs_type_codes.put( new Integer( 26766 ), "PCS_NAD27_Georgia_East" );
    projected_cs_type_codes.put( new Integer( 26767 ), "PCS_NAD27_Georgia_West" );
    projected_cs_type_codes.put( new Integer( 26768 ), "PCS_NAD27_Idaho_East" );
    projected_cs_type_codes.put( new Integer( 26769 ), "PCS_NAD27_Idaho_Central" );
    projected_cs_type_codes.put( new Integer( 26770 ), "PCS_NAD27_Idaho_West" );
    projected_cs_type_codes.put( new Integer( 26771 ), "PCS_NAD27_Illinois_East" );
    projected_cs_type_codes.put( new Integer( 26772 ), "PCS_NAD27_Illinois_West" );
    projected_cs_type_codes.put( new Integer( 26773 ), "PCS_NAD27_Indiana_East" );
    projected_cs_type_codes.put( new Integer( 26774 ), "PCS_NAD27_BLM_14N_feet" );
    projected_cs_type_codes.put( new Integer( 26774 ), "PCS_NAD27_Indiana_West" );
    projected_cs_type_codes.put( new Integer( 26775 ), "PCS_NAD27_BLM_15N_feet" );
    projected_cs_type_codes.put( new Integer( 26775 ), "PCS_NAD27_Iowa_North" );
    projected_cs_type_codes.put( new Integer( 26776 ), "PCS_NAD27_BLM_16N_feet" );
    projected_cs_type_codes.put( new Integer( 26776 ), "PCS_NAD27_Iowa_South" );
    projected_cs_type_codes.put( new Integer( 26777 ), "PCS_NAD27_BLM_17N_feet" );
    projected_cs_type_codes.put( new Integer( 26777 ), "PCS_NAD27_Kansas_North" );
    projected_cs_type_codes.put( new Integer( 26778 ), "PCS_NAD27_Kansas_South" );
    projected_cs_type_codes.put( new Integer( 26779 ), "PCS_NAD27_Kentucky_North" );
    projected_cs_type_codes.put( new Integer( 26780 ), "PCS_NAD27_Kentucky_South" );
    projected_cs_type_codes.put( new Integer( 26781 ), "PCS_NAD27_Louisiana_North" );
    projected_cs_type_codes.put( new Integer( 26782 ), "PCS_NAD27_Louisiana_South" );
    projected_cs_type_codes.put( new Integer( 26783 ), "PCS_NAD27_Maine_East" );
    projected_cs_type_codes.put( new Integer( 26784 ), "PCS_NAD27_Maine_West" );
    projected_cs_type_codes.put( new Integer( 26785 ), "PCS_NAD27_Maryland" );
    projected_cs_type_codes.put( new Integer( 26786 ), "PCS_NAD27_Massachusetts" );
    projected_cs_type_codes.put( new Integer( 26787 ), "PCS_NAD27_Massachusetts_Is" );
    projected_cs_type_codes.put( new Integer( 26788 ), "PCS_NAD27_Michigan_North" );
    projected_cs_type_codes.put( new Integer( 26789 ), "PCS_NAD27_Michigan_Central" );
    projected_cs_type_codes.put( new Integer( 26790 ), "PCS_NAD27_Michigan_South" );
    projected_cs_type_codes.put( new Integer( 26791 ), "PCS_NAD27_Minnesota_North" );
    projected_cs_type_codes.put( new Integer( 26792 ), "PCS_NAD27_Minnesota_Cent" );
    projected_cs_type_codes.put( new Integer( 26793 ), "PCS_NAD27_Minnesota_South" );
    projected_cs_type_codes.put( new Integer( 26794 ), "PCS_NAD27_Mississippi_East" );
    projected_cs_type_codes.put( new Integer( 26795 ), "PCS_NAD27_Mississippi_West" );
    projected_cs_type_codes.put( new Integer( 26796 ), "PCS_NAD27_Missouri_East" );
    projected_cs_type_codes.put( new Integer( 26797 ), "PCS_NAD27_Missouri_Central" );
    projected_cs_type_codes.put( new Integer( 26798 ), "PCS_NAD27_Missouri_West" );
    projected_cs_type_codes.put( new Integer( 26801 ), "PCS_NAD_Michigan_Michigan_East" );
    projected_cs_type_codes.put( new Integer( 26802 ), "PCS_NAD_Michigan_Michigan_Old_Central" );
    projected_cs_type_codes.put( new Integer( 26803 ), "PCS_NAD_Michigan_Michigan_West" );
    projected_cs_type_codes.put( new Integer( 26903 ), "PCS_NAD83_UTM_zone_3N" );
    projected_cs_type_codes.put( new Integer( 26904 ), "PCS_NAD83_UTM_zone_4N" );
    projected_cs_type_codes.put( new Integer( 26905 ), "PCS_NAD83_UTM_zone_5N" );
    projected_cs_type_codes.put( new Integer( 26906 ), "PCS_NAD83_UTM_zone_6N" );
    projected_cs_type_codes.put( new Integer( 26907 ), "PCS_NAD83_UTM_zone_7N" );
    projected_cs_type_codes.put( new Integer( 26908 ), "PCS_NAD83_UTM_zone_8N" );
    projected_cs_type_codes.put( new Integer( 26909 ), "PCS_NAD83_UTM_zone_9N" );
    projected_cs_type_codes.put( new Integer( 26910 ), "PCS_NAD83_UTM_zone_10N" );
    projected_cs_type_codes.put( new Integer( 26911 ), "PCS_NAD83_UTM_zone_11N" );
    projected_cs_type_codes.put( new Integer( 26912 ), "PCS_NAD83_UTM_zone_12N" );
    projected_cs_type_codes.put( new Integer( 26913 ), "PCS_NAD83_UTM_zone_13N" );
    projected_cs_type_codes.put( new Integer( 26914 ), "PCS_NAD83_UTM_zone_14N" );
    projected_cs_type_codes.put( new Integer( 26915 ), "PCS_NAD83_UTM_zone_15N" );
    projected_cs_type_codes.put( new Integer( 26916 ), "PCS_NAD83_UTM_zone_16N" );
    projected_cs_type_codes.put( new Integer( 26917 ), "PCS_NAD83_UTM_zone_17N" );
    projected_cs_type_codes.put( new Integer( 26918 ), "PCS_NAD83_UTM_zone_18N" );
    projected_cs_type_codes.put( new Integer( 26919 ), "PCS_NAD83_UTM_zone_19N" );
    projected_cs_type_codes.put( new Integer( 26920 ), "PCS_NAD83_UTM_zone_20N" );
    projected_cs_type_codes.put( new Integer( 26921 ), "PCS_NAD83_UTM_zone_21N" );
    projected_cs_type_codes.put( new Integer( 26922 ), "PCS_NAD83_UTM_zone_22N" );
    projected_cs_type_codes.put( new Integer( 26923 ), "PCS_NAD83_UTM_zone_23N" );
    projected_cs_type_codes.put( new Integer( 26929 ), "PCS_NAD83_Alabama_East" );
    projected_cs_type_codes.put( new Integer( 26930 ), "PCS_NAD83_Alabama_West" );
    projected_cs_type_codes.put( new Integer( 26931 ), "PCS_NAD83_Alaska_zone_1" );
    projected_cs_type_codes.put( new Integer( 26932 ), "PCS_NAD83_Alaska_zone_2" );
    projected_cs_type_codes.put( new Integer( 26933 ), "PCS_NAD83_Alaska_zone_3" );
    projected_cs_type_codes.put( new Integer( 26934 ), "PCS_NAD83_Alaska_zone_4" );
    projected_cs_type_codes.put( new Integer( 26935 ), "PCS_NAD83_Alaska_zone_5" );
    projected_cs_type_codes.put( new Integer( 26936 ), "PCS_NAD83_Alaska_zone_6" );
    projected_cs_type_codes.put( new Integer( 26937 ), "PCS_NAD83_Alaska_zone_7" );
    projected_cs_type_codes.put( new Integer( 26938 ), "PCS_NAD83_Alaska_zone_8" );
    projected_cs_type_codes.put( new Integer( 26939 ), "PCS_NAD83_Alaska_zone_9" );
    projected_cs_type_codes.put( new Integer( 26940 ), "PCS_NAD83_Alaska_zone_10" );
    projected_cs_type_codes.put( new Integer( 26941 ), "PCS_NAD83_California_1" );
    projected_cs_type_codes.put( new Integer( 26942 ), "PCS_NAD83_California_2" );
    projected_cs_type_codes.put( new Integer( 26943 ), "PCS_NAD83_California_3" );
    projected_cs_type_codes.put( new Integer( 26944 ), "PCS_NAD83_California_4" );
    projected_cs_type_codes.put( new Integer( 26945 ), "PCS_NAD83_California_5" );
    projected_cs_type_codes.put( new Integer( 26946 ), "PCS_NAD83_California_6" );
    projected_cs_type_codes.put( new Integer( 26948 ), "PCS_NAD83_Arizona_East" );
    projected_cs_type_codes.put( new Integer( 26949 ), "PCS_NAD83_Arizona_Central" );
    projected_cs_type_codes.put( new Integer( 26950 ), "PCS_NAD83_Arizona_West" );
    projected_cs_type_codes.put( new Integer( 26951 ), "PCS_NAD83_Arkansas_North" );
    projected_cs_type_codes.put( new Integer( 26952 ), "PCS_NAD83_Arkansas_South" );
    projected_cs_type_codes.put( new Integer( 26953 ), "PCS_NAD83_Colorado_North" );
    projected_cs_type_codes.put( new Integer( 26954 ), "PCS_NAD83_Colorado_Central" );
    projected_cs_type_codes.put( new Integer( 26955 ), "PCS_NAD83_Colorado_South" );
    projected_cs_type_codes.put( new Integer( 26956 ), "PCS_NAD83_Connecticut" );
    projected_cs_type_codes.put( new Integer( 26957 ), "PCS_NAD83_Delaware" );
    projected_cs_type_codes.put( new Integer( 26958 ), "PCS_NAD83_Florida_East" );
    projected_cs_type_codes.put( new Integer( 26959 ), "PCS_NAD83_Florida_West" );
    projected_cs_type_codes.put( new Integer( 26960 ), "PCS_NAD83_Florida_North" );
    projected_cs_type_codes.put( new Integer( 26961 ), "PCS_NAD83_Hawaii_zone_1" );
    projected_cs_type_codes.put( new Integer( 26962 ), "PCS_NAD83_Hawaii_zone_2" );
    projected_cs_type_codes.put( new Integer( 26963 ), "PCS_NAD83_Hawaii_zone_3" );
    projected_cs_type_codes.put( new Integer( 26964 ), "PCS_NAD83_Hawaii_zone_4" );
    projected_cs_type_codes.put( new Integer( 26965 ), "PCS_NAD83_Hawaii_zone_5" );
    projected_cs_type_codes.put( new Integer( 26966 ), "PCS_NAD83_Georgia_East" );
    projected_cs_type_codes.put( new Integer( 26967 ), "PCS_NAD83_Georgia_West" );
    projected_cs_type_codes.put( new Integer( 26968 ), "PCS_NAD83_Idaho_East" );
    projected_cs_type_codes.put( new Integer( 26969 ), "PCS_NAD83_Idaho_Central" );
    projected_cs_type_codes.put( new Integer( 26970 ), "PCS_NAD83_Idaho_West" );
    projected_cs_type_codes.put( new Integer( 26971 ), "PCS_NAD83_Illinois_East" );
    projected_cs_type_codes.put( new Integer( 26972 ), "PCS_NAD83_Illinois_West" );
    projected_cs_type_codes.put( new Integer( 26973 ), "PCS_NAD83_Indiana_East" );
    projected_cs_type_codes.put( new Integer( 26974 ), "PCS_NAD83_Indiana_West" );
    projected_cs_type_codes.put( new Integer( 26975 ), "PCS_NAD83_Iowa_North" );
    projected_cs_type_codes.put( new Integer( 26976 ), "PCS_NAD83_Iowa_South" );
    projected_cs_type_codes.put( new Integer( 26977 ), "PCS_NAD83_Kansas_North" );
    projected_cs_type_codes.put( new Integer( 26978 ), "PCS_NAD83_Kansas_South" );
    projected_cs_type_codes.put( new Integer( 26979 ), "PCS_NAD83_Kentucky_North" );
    projected_cs_type_codes.put( new Integer( 26980 ), "PCS_NAD83_Kentucky_South" );
    projected_cs_type_codes.put( new Integer( 26981 ), "PCS_NAD83_Louisiana_North" );
    projected_cs_type_codes.put( new Integer( 26982 ), "PCS_NAD83_Louisiana_South" );
    projected_cs_type_codes.put( new Integer( 26983 ), "PCS_NAD83_Maine_East" );
    projected_cs_type_codes.put( new Integer( 26984 ), "PCS_NAD83_Maine_West" );
    projected_cs_type_codes.put( new Integer( 26985 ), "PCS_NAD83_Maryland" );
    projected_cs_type_codes.put( new Integer( 26986 ), "PCS_NAD83_Massachusetts" );
    projected_cs_type_codes.put( new Integer( 26987 ), "PCS_NAD83_Massachusetts_Is" );
    projected_cs_type_codes.put( new Integer( 26988 ), "PCS_NAD83_Michigan_North" );
    projected_cs_type_codes.put( new Integer( 26989 ), "PCS_NAD83_Michigan_Central" );
    projected_cs_type_codes.put( new Integer( 26990 ), "PCS_NAD83_Michigan_South" );
    projected_cs_type_codes.put( new Integer( 26991 ), "PCS_NAD83_Minnesota_North" );
    projected_cs_type_codes.put( new Integer( 26992 ), "PCS_NAD83_Minnesota_Cent" );
    projected_cs_type_codes.put( new Integer( 26993 ), "PCS_NAD83_Minnesota_South" );
    projected_cs_type_codes.put( new Integer( 26994 ), "PCS_NAD83_Mississippi_East" );
    projected_cs_type_codes.put( new Integer( 26995 ), "PCS_NAD83_Mississippi_West" );
    projected_cs_type_codes.put( new Integer( 26996 ), "PCS_NAD83_Missouri_East" );
    projected_cs_type_codes.put( new Integer( 26997 ), "PCS_NAD83_Missouri_Central" );
    projected_cs_type_codes.put( new Integer( 26998 ), "PCS_NAD83_Missouri_West" );
    projected_cs_type_codes.put( new Integer( 27038 ), "PCS_Nahrwan_1967_UTM_38N" );
    projected_cs_type_codes.put( new Integer( 27039 ), "PCS_Nahrwan_1967_UTM_39N" );
    projected_cs_type_codes.put( new Integer( 27040 ), "PCS_Nahrwan_1967_UTM_40N" );
    projected_cs_type_codes.put( new Integer( 27120 ), "PCS_Naparima_UTM_20N" );
    projected_cs_type_codes.put( new Integer( 27200 ), "PCS_GD49_NZ_Map_Grid" );
    projected_cs_type_codes.put( new Integer( 27291 ), "PCS_GD49_North_Island_Grid" );
    projected_cs_type_codes.put( new Integer( 27292 ), "PCS_GD49_South_Island_Grid" );
    projected_cs_type_codes.put( new Integer( 27429 ), "PCS_Datum_73_UTM_zone_29N" );
    projected_cs_type_codes.put( new Integer( 27500 ), "PCS_ATF_Nord_de_Guerre" );
    projected_cs_type_codes.put( new Integer( 27581 ), "PCS_NTF_France_I" );
    projected_cs_type_codes.put( new Integer( 27582 ), "PCS_NTF_France_II" );
    projected_cs_type_codes.put( new Integer( 27583 ), "PCS_NTF_France_III" );
    projected_cs_type_codes.put( new Integer( 27591 ), "PCS_NTF_Nord_France" );
    projected_cs_type_codes.put( new Integer( 27592 ), "PCS_NTF_Centre_France" );
    projected_cs_type_codes.put( new Integer( 27593 ), "PCS_NTF_Sud_France" );
    projected_cs_type_codes.put( new Integer( 27700 ), "PCS_British_National_Grid" );
    projected_cs_type_codes.put( new Integer( 28232 ), "PCS_Point_Noire_UTM_32S" );
    projected_cs_type_codes.put( new Integer( 28348 ), "PCS_GDA94_MGA_zone_48" );
    projected_cs_type_codes.put( new Integer( 28349 ), "PCS_GDA94_MGA_zone_49" );
    projected_cs_type_codes.put( new Integer( 28350 ), "PCS_GDA94_MGA_zone_50" );
    projected_cs_type_codes.put( new Integer( 28351 ), "PCS_GDA94_MGA_zone_51" );
    projected_cs_type_codes.put( new Integer( 28352 ), "PCS_GDA94_MGA_zone_52" );
    projected_cs_type_codes.put( new Integer( 28353 ), "PCS_GDA94_MGA_zone_53" );
    projected_cs_type_codes.put( new Integer( 28354 ), "PCS_GDA94_MGA_zone_54" );
    projected_cs_type_codes.put( new Integer( 28355 ), "PCS_GDA94_MGA_zone_55" );
    projected_cs_type_codes.put( new Integer( 28356 ), "PCS_GDA94_MGA_zone_56" );
    projected_cs_type_codes.put( new Integer( 28357 ), "PCS_GDA94_MGA_zone_57" );
    projected_cs_type_codes.put( new Integer( 28358 ), "PCS_GDA94_MGA_zone_58" );
    projected_cs_type_codes.put( new Integer( 28404 ), "PCS_Pulkovo_Gauss_zone_4" );
    projected_cs_type_codes.put( new Integer( 28405 ), "PCS_Pulkovo_Gauss_zone_5" );
    projected_cs_type_codes.put( new Integer( 28406 ), "PCS_Pulkovo_Gauss_zone_6" );
    projected_cs_type_codes.put( new Integer( 28407 ), "PCS_Pulkovo_Gauss_zone_7" );
    projected_cs_type_codes.put( new Integer( 28408 ), "PCS_Pulkovo_Gauss_zone_8" );
    projected_cs_type_codes.put( new Integer( 28409 ), "PCS_Pulkovo_Gauss_zone_9" );
    projected_cs_type_codes.put( new Integer( 28410 ), "PCS_Pulkovo_Gauss_zone_10" );
    projected_cs_type_codes.put( new Integer( 28411 ), "PCS_Pulkovo_Gauss_zone_11" );
    projected_cs_type_codes.put( new Integer( 28412 ), "PCS_Pulkovo_Gauss_zone_12" );
    projected_cs_type_codes.put( new Integer( 28413 ), "PCS_Pulkovo_Gauss_zone_13" );
    projected_cs_type_codes.put( new Integer( 28414 ), "PCS_Pulkovo_Gauss_zone_14" );
    projected_cs_type_codes.put( new Integer( 28415 ), "PCS_Pulkovo_Gauss_zone_15" );
    projected_cs_type_codes.put( new Integer( 28416 ), "PCS_Pulkovo_Gauss_zone_16" );
    projected_cs_type_codes.put( new Integer( 28417 ), "PCS_Pulkovo_Gauss_zone_17" );
    projected_cs_type_codes.put( new Integer( 28418 ), "PCS_Pulkovo_Gauss_zone_18" );
    projected_cs_type_codes.put( new Integer( 28419 ), "PCS_Pulkovo_Gauss_zone_19" );
    projected_cs_type_codes.put( new Integer( 28420 ), "PCS_Pulkovo_Gauss_zone_20" );
    projected_cs_type_codes.put( new Integer( 28421 ), "PCS_Pulkovo_Gauss_zone_21" );
    projected_cs_type_codes.put( new Integer( 28422 ), "PCS_Pulkovo_Gauss_zone_22" );
    projected_cs_type_codes.put( new Integer( 28423 ), "PCS_Pulkovo_Gauss_zone_23" );
    projected_cs_type_codes.put( new Integer( 28424 ), "PCS_Pulkovo_Gauss_zone_24" );
    projected_cs_type_codes.put( new Integer( 28425 ), "PCS_Pulkovo_Gauss_zone_25" );
    projected_cs_type_codes.put( new Integer( 28426 ), "PCS_Pulkovo_Gauss_zone_26" );
    projected_cs_type_codes.put( new Integer( 28427 ), "PCS_Pulkovo_Gauss_zone_27" );
    projected_cs_type_codes.put( new Integer( 28428 ), "PCS_Pulkovo_Gauss_zone_28" );
    projected_cs_type_codes.put( new Integer( 28429 ), "PCS_Pulkovo_Gauss_zone_29" );
    projected_cs_type_codes.put( new Integer( 28430 ), "PCS_Pulkovo_Gauss_zone_30" );
    projected_cs_type_codes.put( new Integer( 28431 ), "PCS_Pulkovo_Gauss_zone_31" );
    projected_cs_type_codes.put( new Integer( 28432 ), "PCS_Pulkovo_Gauss_zone_32" );
    projected_cs_type_codes.put( new Integer( 28464 ), "PCS_Pulkovo_Gauss_4N" );
    projected_cs_type_codes.put( new Integer( 28465 ), "PCS_Pulkovo_Gauss_5N" );
    projected_cs_type_codes.put( new Integer( 28466 ), "PCS_Pulkovo_Gauss_6N" );
    projected_cs_type_codes.put( new Integer( 28467 ), "PCS_Pulkovo_Gauss_7N" );
    projected_cs_type_codes.put( new Integer( 28468 ), "PCS_Pulkovo_Gauss_8N" );
    projected_cs_type_codes.put( new Integer( 28469 ), "PCS_Pulkovo_Gauss_9N" );
    projected_cs_type_codes.put( new Integer( 28470 ), "PCS_Pulkovo_Gauss_10N" );
    projected_cs_type_codes.put( new Integer( 28471 ), "PCS_Pulkovo_Gauss_11N" );
    projected_cs_type_codes.put( new Integer( 28472 ), "PCS_Pulkovo_Gauss_12N" );
    projected_cs_type_codes.put( new Integer( 28473 ), "PCS_Pulkovo_Gauss_13N" );
    projected_cs_type_codes.put( new Integer( 28474 ), "PCS_Pulkovo_Gauss_14N" );
    projected_cs_type_codes.put( new Integer( 28475 ), "PCS_Pulkovo_Gauss_15N" );
    projected_cs_type_codes.put( new Integer( 28476 ), "PCS_Pulkovo_Gauss_16N" );
    projected_cs_type_codes.put( new Integer( 28477 ), "PCS_Pulkovo_Gauss_17N" );
    projected_cs_type_codes.put( new Integer( 28478 ), "PCS_Pulkovo_Gauss_18N" );
    projected_cs_type_codes.put( new Integer( 28479 ), "PCS_Pulkovo_Gauss_19N" );
    projected_cs_type_codes.put( new Integer( 28480 ), "PCS_Pulkovo_Gauss_20N" );
    projected_cs_type_codes.put( new Integer( 28481 ), "PCS_Pulkovo_Gauss_21N" );
    projected_cs_type_codes.put( new Integer( 28482 ), "PCS_Pulkovo_Gauss_22N" );
    projected_cs_type_codes.put( new Integer( 28483 ), "PCS_Pulkovo_Gauss_23N" );
    projected_cs_type_codes.put( new Integer( 28484 ), "PCS_Pulkovo_Gauss_24N" );
    projected_cs_type_codes.put( new Integer( 28485 ), "PCS_Pulkovo_Gauss_25N" );
    projected_cs_type_codes.put( new Integer( 28486 ), "PCS_Pulkovo_Gauss_26N" );
    projected_cs_type_codes.put( new Integer( 28487 ), "PCS_Pulkovo_Gauss_27N" );
    projected_cs_type_codes.put( new Integer( 28488 ), "PCS_Pulkovo_Gauss_28N" );
    projected_cs_type_codes.put( new Integer( 28489 ), "PCS_Pulkovo_Gauss_29N" );
    projected_cs_type_codes.put( new Integer( 28490 ), "PCS_Pulkovo_Gauss_30N" );
    projected_cs_type_codes.put( new Integer( 28491 ), "PCS_Pulkovo_Gauss_31N" );
    projected_cs_type_codes.put( new Integer( 28492 ), "PCS_Pulkovo_Gauss_32N" );
    projected_cs_type_codes.put( new Integer( 28600 ), "PCS_Qatar_National_Grid" );
    projected_cs_type_codes.put( new Integer( 28991 ), "PCS_RD_Netherlands_Old" );
    projected_cs_type_codes.put( new Integer( 28992 ), "PCS_RD_Netherlands_New" );
    projected_cs_type_codes.put( new Integer( 29118 ), "PCS_SAD69_UTM_zone_18N" );
    projected_cs_type_codes.put( new Integer( 29119 ), "PCS_SAD69_UTM_zone_19N" );
    projected_cs_type_codes.put( new Integer( 29120 ), "PCS_SAD69_UTM_zone_20N" );
    projected_cs_type_codes.put( new Integer( 29121 ), "PCS_SAD69_UTM_zone_21N" );
    projected_cs_type_codes.put( new Integer( 29122 ), "PCS_SAD69_UTM_zone_22N" );
    projected_cs_type_codes.put( new Integer( 29177 ), "PCS_SAD69_UTM_zone_17S" );
    projected_cs_type_codes.put( new Integer( 29178 ), "PCS_SAD69_UTM_zone_18S" );
    projected_cs_type_codes.put( new Integer( 29179 ), "PCS_SAD69_UTM_zone_19S" );
    projected_cs_type_codes.put( new Integer( 29180 ), "PCS_SAD69_UTM_zone_20S" );
    projected_cs_type_codes.put( new Integer( 29181 ), "PCS_SAD69_UTM_zone_21S" );
    projected_cs_type_codes.put( new Integer( 29182 ), "PCS_SAD69_UTM_zone_22S" );
    projected_cs_type_codes.put( new Integer( 29183 ), "PCS_SAD69_UTM_zone_23S" );
    projected_cs_type_codes.put( new Integer( 29184 ), "PCS_SAD69_UTM_zone_24S" );
    projected_cs_type_codes.put( new Integer( 29185 ), "PCS_SAD69_UTM_zone_25S" );
    projected_cs_type_codes.put( new Integer( 29220 ), "PCS_Sapper_Hill_UTM_20S" );
    projected_cs_type_codes.put( new Integer( 29221 ), "PCS_Sapper_Hill_UTM_21S" );
    projected_cs_type_codes.put( new Integer( 29333 ), "PCS_Schwarzeck_UTM_33S" );
    projected_cs_type_codes.put( new Integer( 29635 ), "PCS_Sudan_UTM_zone_35N" );
    projected_cs_type_codes.put( new Integer( 29636 ), "PCS_Sudan_UTM_zone_36N" );
    projected_cs_type_codes.put( new Integer( 29700 ), "PCS_Tananarive_Laborde" );
    projected_cs_type_codes.put( new Integer( 29738 ), "PCS_Tananarive_UTM_38S" );
    projected_cs_type_codes.put( new Integer( 29739 ), "PCS_Tananarive_UTM_39S" );
    projected_cs_type_codes.put( new Integer( 29800 ), "PCS_Timbalai_1948_Borneo" );
    projected_cs_type_codes.put( new Integer( 29849 ), "PCS_Timbalai_1948_UTM_49N" );
    projected_cs_type_codes.put( new Integer( 29850 ), "PCS_Timbalai_1948_UTM_50N" );
    projected_cs_type_codes.put( new Integer( 29900 ), "PCS_TM65_Irish_Nat_Grid" );
    projected_cs_type_codes.put( new Integer( 30200 ), "PCS_Trinidad_1903_Trinidad" );
    projected_cs_type_codes.put( new Integer( 30339 ), "PCS_TC_1948_UTM_zone_39N" );
    projected_cs_type_codes.put( new Integer( 30340 ), "PCS_TC_1948_UTM_zone_40N" );
    projected_cs_type_codes.put( new Integer( 30491 ), "PCS_Voirol_N_Algerie_ancien" );
    projected_cs_type_codes.put( new Integer( 30492 ), "PCS_Voirol_S_Algerie_ancien" );
    projected_cs_type_codes.put( new Integer( 30591 ), "PCS_Voirol_Unifie_N_Algerie" );
    projected_cs_type_codes.put( new Integer( 30592 ), "PCS_Voirol_Unifie_S_Algerie" );
    projected_cs_type_codes.put( new Integer( 30600 ), "PCS_Bern_1938_Swiss_New" );
    projected_cs_type_codes.put( new Integer( 30729 ), "PCS_Nord_Sahara_UTM_29N" );
    projected_cs_type_codes.put( new Integer( 30730 ), "PCS_Nord_Sahara_UTM_30N" );
    projected_cs_type_codes.put( new Integer( 30731 ), "PCS_Nord_Sahara_UTM_31N" );
    projected_cs_type_codes.put( new Integer( 30732 ), "PCS_Nord_Sahara_UTM_32N" );
    projected_cs_type_codes.put( new Integer( 31028 ), "PCS_Yoff_UTM_zone_28N" );
    projected_cs_type_codes.put( new Integer( 31121 ), "PCS_Zanderij_UTM_zone_21N" );
    projected_cs_type_codes.put( new Integer( 31291 ), "PCS_MGI_Austria_West" );
    projected_cs_type_codes.put( new Integer( 31292 ), "PCS_MGI_Austria_Central" );
    projected_cs_type_codes.put( new Integer( 31293 ), "PCS_MGI_Austria_East" );
    projected_cs_type_codes.put( new Integer( 31300 ), "PCS_Belge_Lambert_72" );
    projected_cs_type_codes.put( new Integer( 31491 ), "PCS_DHDN_Germany_zone_1" );
    projected_cs_type_codes.put( new Integer( 31492 ), "PCS_DHDN_Germany_zone_2" );
    projected_cs_type_codes.put( new Integer( 31493 ), "PCS_DHDN_Germany_zone_3" );
    projected_cs_type_codes.put( new Integer( 31494 ), "PCS_DHDN_Germany_zone_4" );
    projected_cs_type_codes.put( new Integer( 31495 ), "PCS_DHDN_Germany_zone_5" );
    projected_cs_type_codes.put( new Integer( 32001 ), "PCS_NAD27_Montana_North" );
    projected_cs_type_codes.put( new Integer( 32002 ), "PCS_NAD27_Montana_Central" );
    projected_cs_type_codes.put( new Integer( 32003 ), "PCS_NAD27_Montana_South" );
    projected_cs_type_codes.put( new Integer( 32005 ), "PCS_NAD27_Nebraska_North" );
    projected_cs_type_codes.put( new Integer( 32006 ), "PCS_NAD27_Nebraska_South" );
    projected_cs_type_codes.put( new Integer( 32007 ), "PCS_NAD27_Nevada_East" );
    projected_cs_type_codes.put( new Integer( 32008 ), "PCS_NAD27_Nevada_Central" );
    projected_cs_type_codes.put( new Integer( 32009 ), "PCS_NAD27_Nevada_West" );
    projected_cs_type_codes.put( new Integer( 32010 ), "PCS_NAD27_New_Hampshire" );
    projected_cs_type_codes.put( new Integer( 32011 ), "PCS_NAD27_New_Jersey" );
    projected_cs_type_codes.put( new Integer( 32012 ), "PCS_NAD27_New_Mexico_East" );
    projected_cs_type_codes.put( new Integer( 32013 ), "PCS_NAD27_New_Mexico_Cent" );
    projected_cs_type_codes.put( new Integer( 32014 ), "PCS_NAD27_New_Mexico_West" );
    projected_cs_type_codes.put( new Integer( 32015 ), "PCS_NAD27_New_York_East" );
    projected_cs_type_codes.put( new Integer( 32016 ), "PCS_NAD27_New_York_Central" );
    projected_cs_type_codes.put( new Integer( 32017 ), "PCS_NAD27_New_York_West" );
    projected_cs_type_codes.put( new Integer( 32018 ), "PCS_NAD27_New_York_Long_Is" );
    projected_cs_type_codes.put( new Integer( 32019 ), "PCS_NAD27_North_Carolina" );
    projected_cs_type_codes.put( new Integer( 32020 ), "PCS_NAD27_North_Dakota_N" );
    projected_cs_type_codes.put( new Integer( 32021 ), "PCS_NAD27_North_Dakota_S" );
    projected_cs_type_codes.put( new Integer( 32022 ), "PCS_NAD27_Ohio_North" );
    projected_cs_type_codes.put( new Integer( 32023 ), "PCS_NAD27_Ohio_South" );
    projected_cs_type_codes.put( new Integer( 32024 ), "PCS_NAD27_Oklahoma_North" );
    projected_cs_type_codes.put( new Integer( 32025 ), "PCS_NAD27_Oklahoma_South" );
    projected_cs_type_codes.put( new Integer( 32026 ), "PCS_NAD27_Oregon_North" );
    projected_cs_type_codes.put( new Integer( 32027 ), "PCS_NAD27_Oregon_South" );
    projected_cs_type_codes.put( new Integer( 32028 ), "PCS_NAD27_Pennsylvania_N" );
    projected_cs_type_codes.put( new Integer( 32029 ), "PCS_NAD27_Pennsylvania_S" );
    projected_cs_type_codes.put( new Integer( 32030 ), "PCS_NAD27_Rhode_Island" );
    projected_cs_type_codes.put( new Integer( 32031 ), "PCS_NAD27_South_Carolina_N" );
    projected_cs_type_codes.put( new Integer( 32033 ), "PCS_NAD27_South_Carolina_S" );
    projected_cs_type_codes.put( new Integer( 32034 ), "PCS_NAD27_South_Dakota_N" );
    projected_cs_type_codes.put( new Integer( 32035 ), "PCS_NAD27_South_Dakota_S" );
    projected_cs_type_codes.put( new Integer( 32036 ), "PCS_NAD27_Tennessee" );
    projected_cs_type_codes.put( new Integer( 32037 ), "PCS_NAD27_Texas_North" );
    projected_cs_type_codes.put( new Integer( 32038 ), "PCS_NAD27_Texas_North_Cen" );
    projected_cs_type_codes.put( new Integer( 32039 ), "PCS_NAD27_Texas_Central" );
    projected_cs_type_codes.put( new Integer( 32040 ), "PCS_NAD27_Texas_South_Cen" );
    projected_cs_type_codes.put( new Integer( 32041 ), "PCS_NAD27_Texas_South" );
    projected_cs_type_codes.put( new Integer( 32042 ), "PCS_NAD27_Utah_North" );
    projected_cs_type_codes.put( new Integer( 32043 ), "PCS_NAD27_Utah_Central" );
    projected_cs_type_codes.put( new Integer( 32044 ), "PCS_NAD27_Utah_South" );
    projected_cs_type_codes.put( new Integer( 32045 ), "PCS_NAD27_Vermont" );
    projected_cs_type_codes.put( new Integer( 32046 ), "PCS_NAD27_Virginia_North" );
    projected_cs_type_codes.put( new Integer( 32047 ), "PCS_NAD27_Virginia_South" );
    projected_cs_type_codes.put( new Integer( 32048 ), "PCS_NAD27_Washington_North" );
    projected_cs_type_codes.put( new Integer( 32049 ), "PCS_NAD27_Washington_South" );
    projected_cs_type_codes.put( new Integer( 32050 ), "PCS_NAD27_West_Virginia_N" );
    projected_cs_type_codes.put( new Integer( 32051 ), "PCS_NAD27_West_Virginia_S" );
    projected_cs_type_codes.put( new Integer( 32052 ), "PCS_NAD27_Wisconsin_North" );
    projected_cs_type_codes.put( new Integer( 32053 ), "PCS_NAD27_Wisconsin_Cen" );
    projected_cs_type_codes.put( new Integer( 32054 ), "PCS_NAD27_Wisconsin_South" );
    projected_cs_type_codes.put( new Integer( 32055 ), "PCS_NAD27_Wyoming_East" );
    projected_cs_type_codes.put( new Integer( 32056 ), "PCS_NAD27_Wyoming_E_Cen" );
    projected_cs_type_codes.put( new Integer( 32057 ), "PCS_NAD27_Wyoming_W_Cen" );
    projected_cs_type_codes.put( new Integer( 32058 ), "PCS_NAD27_Wyoming_West" );
    projected_cs_type_codes.put( new Integer( 32059 ), "PCS_NAD27_Puerto_Rico" );
    projected_cs_type_codes.put( new Integer( 32060 ), "PCS_NAD27_St_Croix" );
    projected_cs_type_codes.put( new Integer( 32100 ), "PCS_NAD83_Montana" );
    projected_cs_type_codes.put( new Integer( 32104 ), "PCS_NAD83_Nebraska" );
    projected_cs_type_codes.put( new Integer( 32107 ), "PCS_NAD83_Nevada_East" );
    projected_cs_type_codes.put( new Integer( 32108 ), "PCS_NAD83_Nevada_Central" );
    projected_cs_type_codes.put( new Integer( 32109 ), "PCS_NAD83_Nevada_West" );
    projected_cs_type_codes.put( new Integer( 32110 ), "PCS_NAD83_New_Hampshire" );
    projected_cs_type_codes.put( new Integer( 32111 ), "PCS_NAD83_New_Jersey" );
    projected_cs_type_codes.put( new Integer( 32112 ), "PCS_NAD83_New_Mexico_East" );
    projected_cs_type_codes.put( new Integer( 32113 ), "PCS_NAD83_New_Mexico_Cent" );
    projected_cs_type_codes.put( new Integer( 32114 ), "PCS_NAD83_New_Mexico_West" );
    projected_cs_type_codes.put( new Integer( 32115 ), "PCS_NAD83_New_York_East" );
    projected_cs_type_codes.put( new Integer( 32116 ), "PCS_NAD83_New_York_Central" );
    projected_cs_type_codes.put( new Integer( 32117 ), "PCS_NAD83_New_York_West" );
    projected_cs_type_codes.put( new Integer( 32118 ), "PCS_NAD83_New_York_Long_Is" );
    projected_cs_type_codes.put( new Integer( 32119 ), "PCS_NAD83_North_Carolina" );
    projected_cs_type_codes.put( new Integer( 32120 ), "PCS_NAD83_North_Dakota_N" );
    projected_cs_type_codes.put( new Integer( 32121 ), "PCS_NAD83_North_Dakota_S" );
    projected_cs_type_codes.put( new Integer( 32122 ), "PCS_NAD83_Ohio_North" );
    projected_cs_type_codes.put( new Integer( 32123 ), "PCS_NAD83_Ohio_South" );
    projected_cs_type_codes.put( new Integer( 32124 ), "PCS_NAD83_Oklahoma_North" );
    projected_cs_type_codes.put( new Integer( 32125 ), "PCS_NAD83_Oklahoma_South" );
    projected_cs_type_codes.put( new Integer( 32126 ), "PCS_NAD83_Oregon_North" );
    projected_cs_type_codes.put( new Integer( 32127 ), "PCS_NAD83_Oregon_South" );
    projected_cs_type_codes.put( new Integer( 32128 ), "PCS_NAD83_Pennsylvania_N" );
    projected_cs_type_codes.put( new Integer( 32129 ), "PCS_NAD83_Pennsylvania_S" );
    projected_cs_type_codes.put( new Integer( 32130 ), "PCS_NAD83_Rhode_Island" );
    projected_cs_type_codes.put( new Integer( 32133 ), "PCS_NAD83_South_Carolina" );
    projected_cs_type_codes.put( new Integer( 32134 ), "PCS_NAD83_South_Dakota_N" );
    projected_cs_type_codes.put( new Integer( 32135 ), "PCS_NAD83_South_Dakota_S" );
    projected_cs_type_codes.put( new Integer( 32136 ), "PCS_NAD83_Tennessee" );
    projected_cs_type_codes.put( new Integer( 32137 ), "PCS_NAD83_Texas_North" );
    projected_cs_type_codes.put( new Integer( 32138 ), "PCS_NAD83_Texas_North_Cen" );
    projected_cs_type_codes.put( new Integer( 32139 ), "PCS_NAD83_Texas_Central" );
    projected_cs_type_codes.put( new Integer( 32140 ), "PCS_NAD83_Texas_South_Cen" );
    projected_cs_type_codes.put( new Integer( 32141 ), "PCS_NAD83_Texas_South" );
    projected_cs_type_codes.put( new Integer( 32142 ), "PCS_NAD83_Utah_North" );
    projected_cs_type_codes.put( new Integer( 32143 ), "PCS_NAD83_Utah_Central" );
    projected_cs_type_codes.put( new Integer( 32144 ), "PCS_NAD83_Utah_South" );
    projected_cs_type_codes.put( new Integer( 32145 ), "PCS_NAD83_Vermont" );
    projected_cs_type_codes.put( new Integer( 32146 ), "PCS_NAD83_Virginia_North" );
    projected_cs_type_codes.put( new Integer( 32147 ), "PCS_NAD83_Virginia_South" );
    projected_cs_type_codes.put( new Integer( 32148 ), "PCS_NAD83_Washington_North" );
    projected_cs_type_codes.put( new Integer( 32149 ), "PCS_NAD83_Washington_South" );
    projected_cs_type_codes.put( new Integer( 32150 ), "PCS_NAD83_West_Virginia_N" );
    projected_cs_type_codes.put( new Integer( 32151 ), "PCS_NAD83_West_Virginia_S" );
    projected_cs_type_codes.put( new Integer( 32152 ), "PCS_NAD83_Wisconsin_North" );
    projected_cs_type_codes.put( new Integer( 32153 ), "PCS_NAD83_Wisconsin_Cen" );
    projected_cs_type_codes.put( new Integer( 32154 ), "PCS_NAD83_Wisconsin_South" );
    projected_cs_type_codes.put( new Integer( 32155 ), "PCS_NAD83_Wyoming_East" );
    projected_cs_type_codes.put( new Integer( 32156 ), "PCS_NAD83_Wyoming_E_Cen" );
    projected_cs_type_codes.put( new Integer( 32157 ), "PCS_NAD83_Wyoming_W_Cen" );
    projected_cs_type_codes.put( new Integer( 32158 ), "PCS_NAD83_Wyoming_West" );
    projected_cs_type_codes.put( new Integer( 32161 ), "PCS_NAD83_Puerto_Rico_Virgin_Is" );
    projected_cs_type_codes.put( new Integer( 32201 ), "PCS_WGS72_UTM_zone_1N" );
    projected_cs_type_codes.put( new Integer( 32202 ), "PCS_WGS72_UTM_zone_2N" );
    projected_cs_type_codes.put( new Integer( 32203 ), "PCS_WGS72_UTM_zone_3N" );
    projected_cs_type_codes.put( new Integer( 32204 ), "PCS_WGS72_UTM_zone_4N" );
    projected_cs_type_codes.put( new Integer( 32205 ), "PCS_WGS72_UTM_zone_5N" );
    projected_cs_type_codes.put( new Integer( 32206 ), "PCS_WGS72_UTM_zone_6N" );
    projected_cs_type_codes.put( new Integer( 32207 ), "PCS_WGS72_UTM_zone_7N" );
    projected_cs_type_codes.put( new Integer( 32208 ), "PCS_WGS72_UTM_zone_8N" );
    projected_cs_type_codes.put( new Integer( 32209 ), "PCS_WGS72_UTM_zone_9N" );
    projected_cs_type_codes.put( new Integer( 32210 ), "PCS_WGS72_UTM_zone_10N" );
    projected_cs_type_codes.put( new Integer( 32211 ), "PCS_WGS72_UTM_zone_11N" );
    projected_cs_type_codes.put( new Integer( 32212 ), "PCS_WGS72_UTM_zone_12N" );
    projected_cs_type_codes.put( new Integer( 32213 ), "PCS_WGS72_UTM_zone_13N" );
    projected_cs_type_codes.put( new Integer( 32214 ), "PCS_WGS72_UTM_zone_14N" );
    projected_cs_type_codes.put( new Integer( 32215 ), "PCS_WGS72_UTM_zone_15N" );
    projected_cs_type_codes.put( new Integer( 32216 ), "PCS_WGS72_UTM_zone_16N" );
    projected_cs_type_codes.put( new Integer( 32217 ), "PCS_WGS72_UTM_zone_17N" );
    projected_cs_type_codes.put( new Integer( 32218 ), "PCS_WGS72_UTM_zone_18N" );
    projected_cs_type_codes.put( new Integer( 32219 ), "PCS_WGS72_UTM_zone_19N" );
    projected_cs_type_codes.put( new Integer( 32220 ), "PCS_WGS72_UTM_zone_20N" );
    projected_cs_type_codes.put( new Integer( 32221 ), "PCS_WGS72_UTM_zone_21N" );
    projected_cs_type_codes.put( new Integer( 32222 ), "PCS_WGS72_UTM_zone_22N" );
    projected_cs_type_codes.put( new Integer( 32223 ), "PCS_WGS72_UTM_zone_23N" );
    projected_cs_type_codes.put( new Integer( 32224 ), "PCS_WGS72_UTM_zone_24N" );
    projected_cs_type_codes.put( new Integer( 32225 ), "PCS_WGS72_UTM_zone_25N" );
    projected_cs_type_codes.put( new Integer( 32226 ), "PCS_WGS72_UTM_zone_26N" );
    projected_cs_type_codes.put( new Integer( 32227 ), "PCS_WGS72_UTM_zone_27N" );
    projected_cs_type_codes.put( new Integer( 32228 ), "PCS_WGS72_UTM_zone_28N" );
    projected_cs_type_codes.put( new Integer( 32229 ), "PCS_WGS72_UTM_zone_29N" );
    projected_cs_type_codes.put( new Integer( 32230 ), "PCS_WGS72_UTM_zone_30N" );
    projected_cs_type_codes.put( new Integer( 32231 ), "PCS_WGS72_UTM_zone_31N" );
    projected_cs_type_codes.put( new Integer( 32232 ), "PCS_WGS72_UTM_zone_32N" );
    projected_cs_type_codes.put( new Integer( 32233 ), "PCS_WGS72_UTM_zone_33N" );
    projected_cs_type_codes.put( new Integer( 32234 ), "PCS_WGS72_UTM_zone_34N" );
    projected_cs_type_codes.put( new Integer( 32235 ), "PCS_WGS72_UTM_zone_35N" );
    projected_cs_type_codes.put( new Integer( 32236 ), "PCS_WGS72_UTM_zone_36N" );
    projected_cs_type_codes.put( new Integer( 32237 ), "PCS_WGS72_UTM_zone_37N" );
    projected_cs_type_codes.put( new Integer( 32238 ), "PCS_WGS72_UTM_zone_38N" );
    projected_cs_type_codes.put( new Integer( 32239 ), "PCS_WGS72_UTM_zone_39N" );
    projected_cs_type_codes.put( new Integer( 32240 ), "PCS_WGS72_UTM_zone_40N" );
    projected_cs_type_codes.put( new Integer( 32241 ), "PCS_WGS72_UTM_zone_41N" );
    projected_cs_type_codes.put( new Integer( 32242 ), "PCS_WGS72_UTM_zone_42N" );
    projected_cs_type_codes.put( new Integer( 32243 ), "PCS_WGS72_UTM_zone_43N" );
    projected_cs_type_codes.put( new Integer( 32244 ), "PCS_WGS72_UTM_zone_44N" );
    projected_cs_type_codes.put( new Integer( 32245 ), "PCS_WGS72_UTM_zone_45N" );
    projected_cs_type_codes.put( new Integer( 32246 ), "PCS_WGS72_UTM_zone_46N" );
    projected_cs_type_codes.put( new Integer( 32247 ), "PCS_WGS72_UTM_zone_47N" );
    projected_cs_type_codes.put( new Integer( 32248 ), "PCS_WGS72_UTM_zone_48N" );
    projected_cs_type_codes.put( new Integer( 32249 ), "PCS_WGS72_UTM_zone_49N" );
    projected_cs_type_codes.put( new Integer( 32250 ), "PCS_WGS72_UTM_zone_50N" );
    projected_cs_type_codes.put( new Integer( 32251 ), "PCS_WGS72_UTM_zone_51N" );
    projected_cs_type_codes.put( new Integer( 32252 ), "PCS_WGS72_UTM_zone_52N" );
    projected_cs_type_codes.put( new Integer( 32253 ), "PCS_WGS72_UTM_zone_53N" );
    projected_cs_type_codes.put( new Integer( 32254 ), "PCS_WGS72_UTM_zone_54N" );
    projected_cs_type_codes.put( new Integer( 32255 ), "PCS_WGS72_UTM_zone_55N" );
    projected_cs_type_codes.put( new Integer( 32256 ), "PCS_WGS72_UTM_zone_56N" );
    projected_cs_type_codes.put( new Integer( 32257 ), "PCS_WGS72_UTM_zone_57N" );
    projected_cs_type_codes.put( new Integer( 32258 ), "PCS_WGS72_UTM_zone_58N" );
    projected_cs_type_codes.put( new Integer( 32259 ), "PCS_WGS72_UTM_zone_59N" );
    projected_cs_type_codes.put( new Integer( 32260 ), "PCS_WGS72_UTM_zone_60N" );
    projected_cs_type_codes.put( new Integer( 32301 ), "PCS_WGS72_UTM_zone_1S" );
    projected_cs_type_codes.put( new Integer( 32302 ), "PCS_WGS72_UTM_zone_2S" );
    projected_cs_type_codes.put( new Integer( 32303 ), "PCS_WGS72_UTM_zone_3S" );
    projected_cs_type_codes.put( new Integer( 32304 ), "PCS_WGS72_UTM_zone_4S" );
    projected_cs_type_codes.put( new Integer( 32305 ), "PCS_WGS72_UTM_zone_5S" );
    projected_cs_type_codes.put( new Integer( 32306 ), "PCS_WGS72_UTM_zone_6S" );
    projected_cs_type_codes.put( new Integer( 32307 ), "PCS_WGS72_UTM_zone_7S" );
    projected_cs_type_codes.put( new Integer( 32308 ), "PCS_WGS72_UTM_zone_8S" );
    projected_cs_type_codes.put( new Integer( 32309 ), "PCS_WGS72_UTM_zone_9S" );
    projected_cs_type_codes.put( new Integer( 32310 ), "PCS_WGS72_UTM_zone_10S" );
    projected_cs_type_codes.put( new Integer( 32311 ), "PCS_WGS72_UTM_zone_11S" );
    projected_cs_type_codes.put( new Integer( 32312 ), "PCS_WGS72_UTM_zone_12S" );
    projected_cs_type_codes.put( new Integer( 32313 ), "PCS_WGS72_UTM_zone_13S" );
    projected_cs_type_codes.put( new Integer( 32314 ), "PCS_WGS72_UTM_zone_14S" );
    projected_cs_type_codes.put( new Integer( 32315 ), "PCS_WGS72_UTM_zone_15S" );
    projected_cs_type_codes.put( new Integer( 32316 ), "PCS_WGS72_UTM_zone_16S" );
    projected_cs_type_codes.put( new Integer( 32317 ), "PCS_WGS72_UTM_zone_17S" );
    projected_cs_type_codes.put( new Integer( 32318 ), "PCS_WGS72_UTM_zone_18S" );
    projected_cs_type_codes.put( new Integer( 32319 ), "PCS_WGS72_UTM_zone_19S" );
    projected_cs_type_codes.put( new Integer( 32320 ), "PCS_WGS72_UTM_zone_20S" );
    projected_cs_type_codes.put( new Integer( 32321 ), "PCS_WGS72_UTM_zone_21S" );
    projected_cs_type_codes.put( new Integer( 32322 ), "PCS_WGS72_UTM_zone_22S" );
    projected_cs_type_codes.put( new Integer( 32323 ), "PCS_WGS72_UTM_zone_23S" );
    projected_cs_type_codes.put( new Integer( 32324 ), "PCS_WGS72_UTM_zone_24S" );
    projected_cs_type_codes.put( new Integer( 32325 ), "PCS_WGS72_UTM_zone_25S" );
    projected_cs_type_codes.put( new Integer( 32326 ), "PCS_WGS72_UTM_zone_26S" );
    projected_cs_type_codes.put( new Integer( 32327 ), "PCS_WGS72_UTM_zone_27S" );
    projected_cs_type_codes.put( new Integer( 32328 ), "PCS_WGS72_UTM_zone_28S" );
    projected_cs_type_codes.put( new Integer( 32329 ), "PCS_WGS72_UTM_zone_29S" );
    projected_cs_type_codes.put( new Integer( 32330 ), "PCS_WGS72_UTM_zone_30S" );
    projected_cs_type_codes.put( new Integer( 32331 ), "PCS_WGS72_UTM_zone_31S" );
    projected_cs_type_codes.put( new Integer( 32332 ), "PCS_WGS72_UTM_zone_32S" );
    projected_cs_type_codes.put( new Integer( 32333 ), "PCS_WGS72_UTM_zone_33S" );
    projected_cs_type_codes.put( new Integer( 32334 ), "PCS_WGS72_UTM_zone_34S" );
    projected_cs_type_codes.put( new Integer( 32335 ), "PCS_WGS72_UTM_zone_35S" );
    projected_cs_type_codes.put( new Integer( 32336 ), "PCS_WGS72_UTM_zone_36S" );
    projected_cs_type_codes.put( new Integer( 32337 ), "PCS_WGS72_UTM_zone_37S" );
    projected_cs_type_codes.put( new Integer( 32338 ), "PCS_WGS72_UTM_zone_38S" );
    projected_cs_type_codes.put( new Integer( 32339 ), "PCS_WGS72_UTM_zone_39S" );
    projected_cs_type_codes.put( new Integer( 32340 ), "PCS_WGS72_UTM_zone_40S" );
    projected_cs_type_codes.put( new Integer( 32341 ), "PCS_WGS72_UTM_zone_41S" );
    projected_cs_type_codes.put( new Integer( 32342 ), "PCS_WGS72_UTM_zone_42S" );
    projected_cs_type_codes.put( new Integer( 32343 ), "PCS_WGS72_UTM_zone_43S" );
    projected_cs_type_codes.put( new Integer( 32344 ), "PCS_WGS72_UTM_zone_44S" );
    projected_cs_type_codes.put( new Integer( 32345 ), "PCS_WGS72_UTM_zone_45S" );
    projected_cs_type_codes.put( new Integer( 32346 ), "PCS_WGS72_UTM_zone_46S" );
    projected_cs_type_codes.put( new Integer( 32347 ), "PCS_WGS72_UTM_zone_47S" );
    projected_cs_type_codes.put( new Integer( 32348 ), "PCS_WGS72_UTM_zone_48S" );
    projected_cs_type_codes.put( new Integer( 32349 ), "PCS_WGS72_UTM_zone_49S" );
    projected_cs_type_codes.put( new Integer( 32350 ), "PCS_WGS72_UTM_zone_50S" );
    projected_cs_type_codes.put( new Integer( 32351 ), "PCS_WGS72_UTM_zone_51S" );
    projected_cs_type_codes.put( new Integer( 32352 ), "PCS_WGS72_UTM_zone_52S" );
    projected_cs_type_codes.put( new Integer( 32353 ), "PCS_WGS72_UTM_zone_53S" );
    projected_cs_type_codes.put( new Integer( 32354 ), "PCS_WGS72_UTM_zone_54S" );
    projected_cs_type_codes.put( new Integer( 32355 ), "PCS_WGS72_UTM_zone_55S" );
    projected_cs_type_codes.put( new Integer( 32356 ), "PCS_WGS72_UTM_zone_56S" );
    projected_cs_type_codes.put( new Integer( 32357 ), "PCS_WGS72_UTM_zone_57S" );
    projected_cs_type_codes.put( new Integer( 32358 ), "PCS_WGS72_UTM_zone_58S" );
    projected_cs_type_codes.put( new Integer( 32359 ), "PCS_WGS72_UTM_zone_59S" );
    projected_cs_type_codes.put( new Integer( 32360 ), "PCS_WGS72_UTM_zone_60S" );
    projected_cs_type_codes.put( new Integer( 32401 ), "PCS_WGS72BE_UTM_zone_1N" );
    projected_cs_type_codes.put( new Integer( 32402 ), "PCS_WGS72BE_UTM_zone_2N" );
    projected_cs_type_codes.put( new Integer( 32403 ), "PCS_WGS72BE_UTM_zone_3N" );
    projected_cs_type_codes.put( new Integer( 32404 ), "PCS_WGS72BE_UTM_zone_4N" );
    projected_cs_type_codes.put( new Integer( 32405 ), "PCS_WGS72BE_UTM_zone_5N" );
    projected_cs_type_codes.put( new Integer( 32406 ), "PCS_WGS72BE_UTM_zone_6N" );
    projected_cs_type_codes.put( new Integer( 32407 ), "PCS_WGS72BE_UTM_zone_7N" );
    projected_cs_type_codes.put( new Integer( 32408 ), "PCS_WGS72BE_UTM_zone_8N" );
    projected_cs_type_codes.put( new Integer( 32409 ), "PCS_WGS72BE_UTM_zone_9N" );
    projected_cs_type_codes.put( new Integer( 32410 ), "PCS_WGS72BE_UTM_zone_10N" );
    projected_cs_type_codes.put( new Integer( 32411 ), "PCS_WGS72BE_UTM_zone_11N" );
    projected_cs_type_codes.put( new Integer( 32412 ), "PCS_WGS72BE_UTM_zone_12N" );
    projected_cs_type_codes.put( new Integer( 32413 ), "PCS_WGS72BE_UTM_zone_13N" );
    projected_cs_type_codes.put( new Integer( 32414 ), "PCS_WGS72BE_UTM_zone_14N" );
    projected_cs_type_codes.put( new Integer( 32415 ), "PCS_WGS72BE_UTM_zone_15N" );
    projected_cs_type_codes.put( new Integer( 32416 ), "PCS_WGS72BE_UTM_zone_16N" );
    projected_cs_type_codes.put( new Integer( 32417 ), "PCS_WGS72BE_UTM_zone_17N" );
    projected_cs_type_codes.put( new Integer( 32418 ), "PCS_WGS72BE_UTM_zone_18N" );
    projected_cs_type_codes.put( new Integer( 32419 ), "PCS_WGS72BE_UTM_zone_19N" );
    projected_cs_type_codes.put( new Integer( 32420 ), "PCS_WGS72BE_UTM_zone_20N" );
    projected_cs_type_codes.put( new Integer( 32421 ), "PCS_WGS72BE_UTM_zone_21N" );
    projected_cs_type_codes.put( new Integer( 32422 ), "PCS_WGS72BE_UTM_zone_22N" );
    projected_cs_type_codes.put( new Integer( 32423 ), "PCS_WGS72BE_UTM_zone_23N" );
    projected_cs_type_codes.put( new Integer( 32424 ), "PCS_WGS72BE_UTM_zone_24N" );
    projected_cs_type_codes.put( new Integer( 32425 ), "PCS_WGS72BE_UTM_zone_25N" );
    projected_cs_type_codes.put( new Integer( 32426 ), "PCS_WGS72BE_UTM_zone_26N" );
    projected_cs_type_codes.put( new Integer( 32427 ), "PCS_WGS72BE_UTM_zone_27N" );
    projected_cs_type_codes.put( new Integer( 32428 ), "PCS_WGS72BE_UTM_zone_28N" );
    projected_cs_type_codes.put( new Integer( 32429 ), "PCS_WGS72BE_UTM_zone_29N" );
    projected_cs_type_codes.put( new Integer( 32430 ), "PCS_WGS72BE_UTM_zone_30N" );
    projected_cs_type_codes.put( new Integer( 32431 ), "PCS_WGS72BE_UTM_zone_31N" );
    projected_cs_type_codes.put( new Integer( 32432 ), "PCS_WGS72BE_UTM_zone_32N" );
    projected_cs_type_codes.put( new Integer( 32433 ), "PCS_WGS72BE_UTM_zone_33N" );
    projected_cs_type_codes.put( new Integer( 32434 ), "PCS_WGS72BE_UTM_zone_34N" );
    projected_cs_type_codes.put( new Integer( 32435 ), "PCS_WGS72BE_UTM_zone_35N" );
    projected_cs_type_codes.put( new Integer( 32436 ), "PCS_WGS72BE_UTM_zone_36N" );
    projected_cs_type_codes.put( new Integer( 32437 ), "PCS_WGS72BE_UTM_zone_37N" );
    projected_cs_type_codes.put( new Integer( 32438 ), "PCS_WGS72BE_UTM_zone_38N" );
    projected_cs_type_codes.put( new Integer( 32439 ), "PCS_WGS72BE_UTM_zone_39N" );
    projected_cs_type_codes.put( new Integer( 32440 ), "PCS_WGS72BE_UTM_zone_40N" );
    projected_cs_type_codes.put( new Integer( 32441 ), "PCS_WGS72BE_UTM_zone_41N" );
    projected_cs_type_codes.put( new Integer( 32442 ), "PCS_WGS72BE_UTM_zone_42N" );
    projected_cs_type_codes.put( new Integer( 32443 ), "PCS_WGS72BE_UTM_zone_43N" );
    projected_cs_type_codes.put( new Integer( 32444 ), "PCS_WGS72BE_UTM_zone_44N" );
    projected_cs_type_codes.put( new Integer( 32445 ), "PCS_WGS72BE_UTM_zone_45N" );
    projected_cs_type_codes.put( new Integer( 32446 ), "PCS_WGS72BE_UTM_zone_46N" );
    projected_cs_type_codes.put( new Integer( 32447 ), "PCS_WGS72BE_UTM_zone_47N" );
    projected_cs_type_codes.put( new Integer( 32448 ), "PCS_WGS72BE_UTM_zone_48N" );
    projected_cs_type_codes.put( new Integer( 32449 ), "PCS_WGS72BE_UTM_zone_49N" );
    projected_cs_type_codes.put( new Integer( 32450 ), "PCS_WGS72BE_UTM_zone_50N" );
    projected_cs_type_codes.put( new Integer( 32451 ), "PCS_WGS72BE_UTM_zone_51N" );
    projected_cs_type_codes.put( new Integer( 32452 ), "PCS_WGS72BE_UTM_zone_52N" );
    projected_cs_type_codes.put( new Integer( 32453 ), "PCS_WGS72BE_UTM_zone_53N" );
    projected_cs_type_codes.put( new Integer( 32454 ), "PCS_WGS72BE_UTM_zone_54N" );
    projected_cs_type_codes.put( new Integer( 32455 ), "PCS_WGS72BE_UTM_zone_55N" );
    projected_cs_type_codes.put( new Integer( 32456 ), "PCS_WGS72BE_UTM_zone_56N" );
    projected_cs_type_codes.put( new Integer( 32457 ), "PCS_WGS72BE_UTM_zone_57N" );
    projected_cs_type_codes.put( new Integer( 32458 ), "PCS_WGS72BE_UTM_zone_58N" );
    projected_cs_type_codes.put( new Integer( 32459 ), "PCS_WGS72BE_UTM_zone_59N" );
    projected_cs_type_codes.put( new Integer( 32460 ), "PCS_WGS72BE_UTM_zone_60N" );
    projected_cs_type_codes.put( new Integer( 32501 ), "PCS_WGS72BE_UTM_zone_1S" );
    projected_cs_type_codes.put( new Integer( 32502 ), "PCS_WGS72BE_UTM_zone_2S" );
    projected_cs_type_codes.put( new Integer( 32503 ), "PCS_WGS72BE_UTM_zone_3S" );
    projected_cs_type_codes.put( new Integer( 32504 ), "PCS_WGS72BE_UTM_zone_4S" );
    projected_cs_type_codes.put( new Integer( 32505 ), "PCS_WGS72BE_UTM_zone_5S" );
    projected_cs_type_codes.put( new Integer( 32506 ), "PCS_WGS72BE_UTM_zone_6S" );
    projected_cs_type_codes.put( new Integer( 32507 ), "PCS_WGS72BE_UTM_zone_7S" );
    projected_cs_type_codes.put( new Integer( 32508 ), "PCS_WGS72BE_UTM_zone_8S" );
    projected_cs_type_codes.put( new Integer( 32509 ), "PCS_WGS72BE_UTM_zone_9S" );
    projected_cs_type_codes.put( new Integer( 32510 ), "PCS_WGS72BE_UTM_zone_10S" );
    projected_cs_type_codes.put( new Integer( 32511 ), "PCS_WGS72BE_UTM_zone_11S" );
    projected_cs_type_codes.put( new Integer( 32512 ), "PCS_WGS72BE_UTM_zone_12S" );
    projected_cs_type_codes.put( new Integer( 32513 ), "PCS_WGS72BE_UTM_zone_13S" );
    projected_cs_type_codes.put( new Integer( 32514 ), "PCS_WGS72BE_UTM_zone_14S" );
    projected_cs_type_codes.put( new Integer( 32515 ), "PCS_WGS72BE_UTM_zone_15S" );
    projected_cs_type_codes.put( new Integer( 32516 ), "PCS_WGS72BE_UTM_zone_16S" );
    projected_cs_type_codes.put( new Integer( 32517 ), "PCS_WGS72BE_UTM_zone_17S" );
    projected_cs_type_codes.put( new Integer( 32518 ), "PCS_WGS72BE_UTM_zone_18S" );
    projected_cs_type_codes.put( new Integer( 32519 ), "PCS_WGS72BE_UTM_zone_19S" );
    projected_cs_type_codes.put( new Integer( 32520 ), "PCS_WGS72BE_UTM_zone_20S" );
    projected_cs_type_codes.put( new Integer( 32521 ), "PCS_WGS72BE_UTM_zone_21S" );
    projected_cs_type_codes.put( new Integer( 32522 ), "PCS_WGS72BE_UTM_zone_22S" );
    projected_cs_type_codes.put( new Integer( 32523 ), "PCS_WGS72BE_UTM_zone_23S" );
    projected_cs_type_codes.put( new Integer( 32524 ), "PCS_WGS72BE_UTM_zone_24S" );
    projected_cs_type_codes.put( new Integer( 32525 ), "PCS_WGS72BE_UTM_zone_25S" );
    projected_cs_type_codes.put( new Integer( 32526 ), "PCS_WGS72BE_UTM_zone_26S" );
    projected_cs_type_codes.put( new Integer( 32527 ), "PCS_WGS72BE_UTM_zone_27S" );
    projected_cs_type_codes.put( new Integer( 32528 ), "PCS_WGS72BE_UTM_zone_28S" );
    projected_cs_type_codes.put( new Integer( 32529 ), "PCS_WGS72BE_UTM_zone_29S" );
    projected_cs_type_codes.put( new Integer( 32530 ), "PCS_WGS72BE_UTM_zone_30S" );
    projected_cs_type_codes.put( new Integer( 32531 ), "PCS_WGS72BE_UTM_zone_31S" );
    projected_cs_type_codes.put( new Integer( 32532 ), "PCS_WGS72BE_UTM_zone_32S" );
    projected_cs_type_codes.put( new Integer( 32533 ), "PCS_WGS72BE_UTM_zone_33S" );
    projected_cs_type_codes.put( new Integer( 32534 ), "PCS_WGS72BE_UTM_zone_34S" );
    projected_cs_type_codes.put( new Integer( 32535 ), "PCS_WGS72BE_UTM_zone_35S" );
    projected_cs_type_codes.put( new Integer( 32536 ), "PCS_WGS72BE_UTM_zone_36S" );
    projected_cs_type_codes.put( new Integer( 32537 ), "PCS_WGS72BE_UTM_zone_37S" );
    projected_cs_type_codes.put( new Integer( 32538 ), "PCS_WGS72BE_UTM_zone_38S" );
    projected_cs_type_codes.put( new Integer( 32539 ), "PCS_WGS72BE_UTM_zone_39S" );
    projected_cs_type_codes.put( new Integer( 32540 ), "PCS_WGS72BE_UTM_zone_40S" );
    projected_cs_type_codes.put( new Integer( 32541 ), "PCS_WGS72BE_UTM_zone_41S" );
    projected_cs_type_codes.put( new Integer( 32542 ), "PCS_WGS72BE_UTM_zone_42S" );
    projected_cs_type_codes.put( new Integer( 32543 ), "PCS_WGS72BE_UTM_zone_43S" );
    projected_cs_type_codes.put( new Integer( 32544 ), "PCS_WGS72BE_UTM_zone_44S" );
    projected_cs_type_codes.put( new Integer( 32545 ), "PCS_WGS72BE_UTM_zone_45S" );
    projected_cs_type_codes.put( new Integer( 32546 ), "PCS_WGS72BE_UTM_zone_46S" );
    projected_cs_type_codes.put( new Integer( 32547 ), "PCS_WGS72BE_UTM_zone_47S" );
    projected_cs_type_codes.put( new Integer( 32548 ), "PCS_WGS72BE_UTM_zone_48S" );
    projected_cs_type_codes.put( new Integer( 32549 ), "PCS_WGS72BE_UTM_zone_49S" );
    projected_cs_type_codes.put( new Integer( 32550 ), "PCS_WGS72BE_UTM_zone_50S" );
    projected_cs_type_codes.put( new Integer( 32551 ), "PCS_WGS72BE_UTM_zone_51S" );
    projected_cs_type_codes.put( new Integer( 32552 ), "PCS_WGS72BE_UTM_zone_52S" );
    projected_cs_type_codes.put( new Integer( 32553 ), "PCS_WGS72BE_UTM_zone_53S" );
    projected_cs_type_codes.put( new Integer( 32554 ), "PCS_WGS72BE_UTM_zone_54S" );
    projected_cs_type_codes.put( new Integer( 32555 ), "PCS_WGS72BE_UTM_zone_55S" );
    projected_cs_type_codes.put( new Integer( 32556 ), "PCS_WGS72BE_UTM_zone_56S" );
    projected_cs_type_codes.put( new Integer( 32557 ), "PCS_WGS72BE_UTM_zone_57S" );
    projected_cs_type_codes.put( new Integer( 32558 ), "PCS_WGS72BE_UTM_zone_58S" );
    projected_cs_type_codes.put( new Integer( 32559 ), "PCS_WGS72BE_UTM_zone_59S" );
    projected_cs_type_codes.put( new Integer( 32560 ), "PCS_WGS72BE_UTM_zone_60S" );
    projected_cs_type_codes.put( new Integer( 32601 ), "PCS_WGS84_UTM_zone_1N" );
    projected_cs_type_codes.put( new Integer( 32602 ), "PCS_WGS84_UTM_zone_2N" );
    projected_cs_type_codes.put( new Integer( 32603 ), "PCS_WGS84_UTM_zone_3N" );
    projected_cs_type_codes.put( new Integer( 32604 ), "PCS_WGS84_UTM_zone_4N" );
    projected_cs_type_codes.put( new Integer( 32605 ), "PCS_WGS84_UTM_zone_5N" );
    projected_cs_type_codes.put( new Integer( 32606 ), "PCS_WGS84_UTM_zone_6N" );
    projected_cs_type_codes.put( new Integer( 32607 ), "PCS_WGS84_UTM_zone_7N" );
    projected_cs_type_codes.put( new Integer( 32608 ), "PCS_WGS84_UTM_zone_8N" );
    projected_cs_type_codes.put( new Integer( 32609 ), "PCS_WGS84_UTM_zone_9N" );
    projected_cs_type_codes.put( new Integer( 32610 ), "PCS_WGS84_UTM_zone_10N" );
    projected_cs_type_codes.put( new Integer( 32611 ), "PCS_WGS84_UTM_zone_11N" );
    projected_cs_type_codes.put( new Integer( 32612 ), "PCS_WGS84_UTM_zone_12N" );
    projected_cs_type_codes.put( new Integer( 32613 ), "PCS_WGS84_UTM_zone_13N" );
    projected_cs_type_codes.put( new Integer( 32614 ), "PCS_WGS84_UTM_zone_14N" );
    projected_cs_type_codes.put( new Integer( 32615 ), "PCS_WGS84_UTM_zone_15N" );
    projected_cs_type_codes.put( new Integer( 32616 ), "PCS_WGS84_UTM_zone_16N" );
    projected_cs_type_codes.put( new Integer( 32617 ), "PCS_WGS84_UTM_zone_17N" );
    projected_cs_type_codes.put( new Integer( 32618 ), "PCS_WGS84_UTM_zone_18N" );
    projected_cs_type_codes.put( new Integer( 32619 ), "PCS_WGS84_UTM_zone_19N" );
    projected_cs_type_codes.put( new Integer( 32620 ), "PCS_WGS84_UTM_zone_20N" );
    projected_cs_type_codes.put( new Integer( 32621 ), "PCS_WGS84_UTM_zone_21N" );
    projected_cs_type_codes.put( new Integer( 32622 ), "PCS_WGS84_UTM_zone_22N" );
    projected_cs_type_codes.put( new Integer( 32623 ), "PCS_WGS84_UTM_zone_23N" );
    projected_cs_type_codes.put( new Integer( 32624 ), "PCS_WGS84_UTM_zone_24N" );
    projected_cs_type_codes.put( new Integer( 32625 ), "PCS_WGS84_UTM_zone_25N" );
    projected_cs_type_codes.put( new Integer( 32626 ), "PCS_WGS84_UTM_zone_26N" );
    projected_cs_type_codes.put( new Integer( 32627 ), "PCS_WGS84_UTM_zone_27N" );
    projected_cs_type_codes.put( new Integer( 32628 ), "PCS_WGS84_UTM_zone_28N" );
    projected_cs_type_codes.put( new Integer( 32629 ), "PCS_WGS84_UTM_zone_29N" );
    projected_cs_type_codes.put( new Integer( 32630 ), "PCS_WGS84_UTM_zone_30N" );
    projected_cs_type_codes.put( new Integer( 32631 ), "PCS_WGS84_UTM_zone_31N" );
    projected_cs_type_codes.put( new Integer( 32632 ), "PCS_WGS84_UTM_zone_32N" );
    projected_cs_type_codes.put( new Integer( 32633 ), "PCS_WGS84_UTM_zone_33N" );
    projected_cs_type_codes.put( new Integer( 32634 ), "PCS_WGS84_UTM_zone_34N" );
    projected_cs_type_codes.put( new Integer( 32635 ), "PCS_WGS84_UTM_zone_35N" );
    projected_cs_type_codes.put( new Integer( 32636 ), "PCS_WGS84_UTM_zone_36N" );
    projected_cs_type_codes.put( new Integer( 32637 ), "PCS_WGS84_UTM_zone_37N" );
    projected_cs_type_codes.put( new Integer( 32638 ), "PCS_WGS84_UTM_zone_38N" );
    projected_cs_type_codes.put( new Integer( 32639 ), "PCS_WGS84_UTM_zone_39N" );
    projected_cs_type_codes.put( new Integer( 32640 ), "PCS_WGS84_UTM_zone_40N" );
    projected_cs_type_codes.put( new Integer( 32641 ), "PCS_WGS84_UTM_zone_41N" );
    projected_cs_type_codes.put( new Integer( 32642 ), "PCS_WGS84_UTM_zone_42N" );
    projected_cs_type_codes.put( new Integer( 32643 ), "PCS_WGS84_UTM_zone_43N" );
    projected_cs_type_codes.put( new Integer( 32644 ), "PCS_WGS84_UTM_zone_44N" );
    projected_cs_type_codes.put( new Integer( 32645 ), "PCS_WGS84_UTM_zone_45N" );
    projected_cs_type_codes.put( new Integer( 32646 ), "PCS_WGS84_UTM_zone_46N" );
    projected_cs_type_codes.put( new Integer( 32647 ), "PCS_WGS84_UTM_zone_47N" );
    projected_cs_type_codes.put( new Integer( 32648 ), "PCS_WGS84_UTM_zone_48N" );
    projected_cs_type_codes.put( new Integer( 32649 ), "PCS_WGS84_UTM_zone_49N" );
    projected_cs_type_codes.put( new Integer( 32650 ), "PCS_WGS84_UTM_zone_50N" );
    projected_cs_type_codes.put( new Integer( 32651 ), "PCS_WGS84_UTM_zone_51N" );
    projected_cs_type_codes.put( new Integer( 32652 ), "PCS_WGS84_UTM_zone_52N" );
    projected_cs_type_codes.put( new Integer( 32653 ), "PCS_WGS84_UTM_zone_53N" );
    projected_cs_type_codes.put( new Integer( 32654 ), "PCS_WGS84_UTM_zone_54N" );
    projected_cs_type_codes.put( new Integer( 32655 ), "PCS_WGS84_UTM_zone_55N" );
    projected_cs_type_codes.put( new Integer( 32656 ), "PCS_WGS84_UTM_zone_56N" );
    projected_cs_type_codes.put( new Integer( 32657 ), "PCS_WGS84_UTM_zone_57N" );
    projected_cs_type_codes.put( new Integer( 32658 ), "PCS_WGS84_UTM_zone_58N" );
    projected_cs_type_codes.put( new Integer( 32659 ), "PCS_WGS84_UTM_zone_59N" );
    projected_cs_type_codes.put( new Integer( 32660 ), "PCS_WGS84_UTM_zone_60N" );
    projected_cs_type_codes.put( new Integer( 32701 ), "PCS_WGS84_UTM_zone_1S" );
    projected_cs_type_codes.put( new Integer( 32702 ), "PCS_WGS84_UTM_zone_2S" );
    projected_cs_type_codes.put( new Integer( 32703 ), "PCS_WGS84_UTM_zone_3S" );
    projected_cs_type_codes.put( new Integer( 32704 ), "PCS_WGS84_UTM_zone_4S" );
    projected_cs_type_codes.put( new Integer( 32705 ), "PCS_WGS84_UTM_zone_5S" );
    projected_cs_type_codes.put( new Integer( 32706 ), "PCS_WGS84_UTM_zone_6S" );
    projected_cs_type_codes.put( new Integer( 32707 ), "PCS_WGS84_UTM_zone_7S" );
    projected_cs_type_codes.put( new Integer( 32708 ), "PCS_WGS84_UTM_zone_8S" );
    projected_cs_type_codes.put( new Integer( 32709 ), "PCS_WGS84_UTM_zone_9S" );
    projected_cs_type_codes.put( new Integer( 32710 ), "PCS_WGS84_UTM_zone_10S" );
    projected_cs_type_codes.put( new Integer( 32711 ), "PCS_WGS84_UTM_zone_11S" );
    projected_cs_type_codes.put( new Integer( 32712 ), "PCS_WGS84_UTM_zone_12S" );
    projected_cs_type_codes.put( new Integer( 32713 ), "PCS_WGS84_UTM_zone_13S" );
    projected_cs_type_codes.put( new Integer( 32714 ), "PCS_WGS84_UTM_zone_14S" );
    projected_cs_type_codes.put( new Integer( 32715 ), "PCS_WGS84_UTM_zone_15S" );
    projected_cs_type_codes.put( new Integer( 32716 ), "PCS_WGS84_UTM_zone_16S" );
    projected_cs_type_codes.put( new Integer( 32717 ), "PCS_WGS84_UTM_zone_17S" );
    projected_cs_type_codes.put( new Integer( 32718 ), "PCS_WGS84_UTM_zone_18S" );
    projected_cs_type_codes.put( new Integer( 32719 ), "PCS_WGS84_UTM_zone_19S" );
    projected_cs_type_codes.put( new Integer( 32720 ), "PCS_WGS84_UTM_zone_20S" );
    projected_cs_type_codes.put( new Integer( 32721 ), "PCS_WGS84_UTM_zone_21S" );
    projected_cs_type_codes.put( new Integer( 32722 ), "PCS_WGS84_UTM_zone_22S" );
    projected_cs_type_codes.put( new Integer( 32723 ), "PCS_WGS84_UTM_zone_23S" );
    projected_cs_type_codes.put( new Integer( 32724 ), "PCS_WGS84_UTM_zone_24S" );
    projected_cs_type_codes.put( new Integer( 32725 ), "PCS_WGS84_UTM_zone_25S" );
    projected_cs_type_codes.put( new Integer( 32726 ), "PCS_WGS84_UTM_zone_26S" );
    projected_cs_type_codes.put( new Integer( 32727 ), "PCS_WGS84_UTM_zone_27S" );
    projected_cs_type_codes.put( new Integer( 32728 ), "PCS_WGS84_UTM_zone_28S" );
    projected_cs_type_codes.put( new Integer( 32729 ), "PCS_WGS84_UTM_zone_29S" );
    projected_cs_type_codes.put( new Integer( 32730 ), "PCS_WGS84_UTM_zone_30S" );
    projected_cs_type_codes.put( new Integer( 32731 ), "PCS_WGS84_UTM_zone_31S" );
    projected_cs_type_codes.put( new Integer( 32732 ), "PCS_WGS84_UTM_zone_32S" );
    projected_cs_type_codes.put( new Integer( 32733 ), "PCS_WGS84_UTM_zone_33S" );
    projected_cs_type_codes.put( new Integer( 32734 ), "PCS_WGS84_UTM_zone_34S" );
    projected_cs_type_codes.put( new Integer( 32735 ), "PCS_WGS84_UTM_zone_35S" );
    projected_cs_type_codes.put( new Integer( 32736 ), "PCS_WGS84_UTM_zone_36S" );
    projected_cs_type_codes.put( new Integer( 32737 ), "PCS_WGS84_UTM_zone_37S" );
    projected_cs_type_codes.put( new Integer( 32738 ), "PCS_WGS84_UTM_zone_38S" );
    projected_cs_type_codes.put( new Integer( 32739 ), "PCS_WGS84_UTM_zone_39S" );
    projected_cs_type_codes.put( new Integer( 32740 ), "PCS_WGS84_UTM_zone_40S" );
    projected_cs_type_codes.put( new Integer( 32741 ), "PCS_WGS84_UTM_zone_41S" );
    projected_cs_type_codes.put( new Integer( 32742 ), "PCS_WGS84_UTM_zone_42S" );
    projected_cs_type_codes.put( new Integer( 32743 ), "PCS_WGS84_UTM_zone_43S" );
    projected_cs_type_codes.put( new Integer( 32744 ), "PCS_WGS84_UTM_zone_44S" );
    projected_cs_type_codes.put( new Integer( 32745 ), "PCS_WGS84_UTM_zone_45S" );
    projected_cs_type_codes.put( new Integer( 32746 ), "PCS_WGS84_UTM_zone_46S" );
    projected_cs_type_codes.put( new Integer( 32747 ), "PCS_WGS84_UTM_zone_47S" );
    projected_cs_type_codes.put( new Integer( 32748 ), "PCS_WGS84_UTM_zone_48S" );
    projected_cs_type_codes.put( new Integer( 32749 ), "PCS_WGS84_UTM_zone_49S" );
    projected_cs_type_codes.put( new Integer( 32750 ), "PCS_WGS84_UTM_zone_50S" );
    projected_cs_type_codes.put( new Integer( 32751 ), "PCS_WGS84_UTM_zone_51S" );
    projected_cs_type_codes.put( new Integer( 32752 ), "PCS_WGS84_UTM_zone_52S" );
    projected_cs_type_codes.put( new Integer( 32753 ), "PCS_WGS84_UTM_zone_53S" );
    projected_cs_type_codes.put( new Integer( 32754 ), "PCS_WGS84_UTM_zone_54S" );
    projected_cs_type_codes.put( new Integer( 32755 ), "PCS_WGS84_UTM_zone_55S" );
    projected_cs_type_codes.put( new Integer( 32756 ), "PCS_WGS84_UTM_zone_56S" );
    projected_cs_type_codes.put( new Integer( 32757 ), "PCS_WGS84_UTM_zone_57S" );
    projected_cs_type_codes.put( new Integer( 32758 ), "PCS_WGS84_UTM_zone_58S" );
    projected_cs_type_codes.put( new Integer( 32759 ), "PCS_WGS84_UTM_zone_59S" );
    projected_cs_type_codes.put( new Integer( 32760 ), "PCS_WGS84_UTM_zone_60S" );
  }

  /**
   * 
   * @param code
   * @return
   */
  public static boolean containsProjectedCSTypeCode( int code )
  {
    return projected_cs_type_codes.containsKey( new Integer( code ) );
  }

  /**
   * checks, if Code is found and returns the appropriate String-value.
   * 
   * @param code
   * @return @throws
   *         GeoTiffException if Code is not found.
   */
  public static String getProjectedCSTypeCode( int code ) throws GeoTiffException
  {
    if( containsProjectedCSTypeCode( code ) )
    {
      String value = (String)projected_cs_type_codes.get( new Integer( code ) );
      return value;
    }
    throw new GeoTiffException( "Error in accessing Projected CS Type Code.\n" + "  Code: " + code
        + " not found." );

  }

}

/*
 * ****************************************************************************
 * Changes to this class. What the people have been up to:
 * 
 * $Log$
 * Revision 1.1  2004/10/07 14:09:20  doemming
 * *** empty log message ***
 *
 * Revision 1.1  2004/09/02 23:56:51  doemming
 * *** empty log message ***
 * Revision 1.1 2004/08/31 12:41:08 doemming
 * *** empty log message *** Revision 1.1 2004/08/18 08:50:17 axel_schaefer no
 * message
 * 
 * ****************************************************************************
 */