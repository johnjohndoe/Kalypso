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
package org.deegree_impl.model.cs;

import java.util.Hashtable;
import java.util.Locale;


/**
 * 
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class ConvenienceCSFactoryFull {
    
    // keys are names (i.e. "EPSG:4326"), values are CoordinateSystems
    private Hashtable systems = new Hashtable(250);

    /**
     * Constructs a new ConvenienceCSFactory.
     */
    public ConvenienceCSFactoryFull() {
        
        ConvenienceCSFactory fac = ConvenienceCSFactory.getInstance();
        
        // 1. add the following geographic CS:
        // EPSG4326 = WGS84                   
        CoordinateSystem cs = fac.getCSByName( "EPSG:4326" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4230 = ED50
        cs = fac.getCSByName( "EPSG:4230" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4231 = ED87
        cs = fac.getCSByName( "EPSG:4231" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4314 = DHDN        
        cs = fac.getCSByName( "EPSG:4314" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4258 = ETRS89
        cs = fac.getCSByName( "EPSG:4258" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:4120" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:4806" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:4801" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:4272" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:4803" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:4308" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:4817" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:4274" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:4322" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:4324" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4124 = RT90;
        cs = fac.getCSByName( "EPSG:4124" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4124 = CH19030
        cs = fac.getCSByName( "EPSG:4149" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4150 = CH1903+
        cs = fac.getCSByName( "EPSG:4150" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4151 = CHTRF95
        cs = fac.getCSByName( "EPSG:4151" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4121 = GGRS87
        cs = fac.getCSByName( "EPSG:4121" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4171 = RGF93
        cs = fac.getCSByName( "EPSG:4171" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4173 = IRENET95
        cs = fac.getCSByName( "EPSG:4173" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4237 = HD72
        cs = fac.getCSByName( "EPSG:4237" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4265 = Monte Mario
        cs = fac.getCSByName( "EPSG:4265" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4275 = NTF
        cs = fac.getCSByName( "EPSG:4275" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4277 = OSGB1936
        cs = fac.getCSByName( "EPSG:4277" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4284 = Pulkovo 1942
        cs = fac.getCSByName( "EPSG:4284" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4289 = Amersfoort
        cs = fac.getCSByName( "EPSG:4289" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4299 = TM65
        cs = fac.getCSByName( "EPSG:4299" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4312 = MGI
        cs = fac.getCSByName( "EPSG:4312" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG4313 = BD72
        cs = fac.getCSByName( "EPSG:4313" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG20790 = CH1903+
        cs = fac.getCSByName( "EPSG:20790" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG21780 = LV03C
        cs = fac.getCSByName( "EPSG:21780" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG21781 = LV03
        cs = fac.getCSByName( "EPSG:21781" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
                
        // EPSG26591= Monte Mario / Italy 1
        cs = fac.getCSByName( "EPSG:20790" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );

        // EPSG26592= Monte Mario / Italy 2
        cs = fac.getCSByName( "EPSG:26592" );   
        systems.put( cs.getName( Locale.ENGLISH ),  cs );

        // EPSG27391 = NGO 1948 (Oslo) / NGO zone I
        cs = fac.getCSByName( "EPSG:27391" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );

        // EPSG27392 = NGO 1948 (Oslo) / NGO zone II
        cs = fac.getCSByName( "EPSG:27392" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );

        // EPSG27393 = NGO 1948 (Oslo) / NGO zone III
        cs = fac.getCSByName( "EPSG:27393" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );

        // EPSG27394 = NGO 1948 (Oslo) / NGO zone IV
        cs = fac.getCSByName( "EPSG:27394" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );

        // EPSG27395 = NGO 1948 (Oslo) / NGO zone V
        cs = fac.getCSByName( "EPSG:27395" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG27396 = NGO 1948 (Oslo) / NGO zone VI
        cs = fac.getCSByName( "EPSG:27396" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG27397 = NGO 1948 (Oslo) / NGO zone VII
        cs = fac.getCSByName( "EPSG:27397" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG27398 = NGO 1948 (Oslo) / NGO zone VIII
        cs = fac.getCSByName( "EPSG:27398" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG27429 = Datum 73 / UTM zone 29N
        cs = fac.getCSByName( "EPSG:27429" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG27700 = OSGB 1936 / British National Grid
        cs = fac.getCSByName( "EPSG:27700" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG28402 = Pulkovo 1942 / Gauss-Kruger zone 2
        cs = fac.getCSByName( "EPSG:28402" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG28403 = Pulkovo 1942 / Gauss-Kruger zone 3
        cs = fac.getCSByName( "EPSG:28403" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG28404 = Pulkovo 1942 / Gauss-Kruger zone 4
        cs = fac.getCSByName( "EPSG:28404" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG28405 = Pulkovo 1942 / Gauss-Kruger zone 5
        cs = fac.getCSByName( "EPSG:28405" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG28406 = Pulkovo 1942 / Gauss-Kruger zone 6
        cs = fac.getCSByName( "EPSG:28406" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG28407 = Pulkovo 1942 / Gauss-Kruger zone 7
        cs = fac.getCSByName( "EPSG:28407" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG28408 = Pulkovo 1942 / Gauss-Kruger zone 8
        cs = fac.getCSByName( "EPSG:28408" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG28409 = Pulkovo 1942 / Gauss-Kruger zone 9
        cs = fac.getCSByName( "EPSG:28409" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG29900 = TM65 / Irish National Grid
        cs = fac.getCSByName( "EPSG:29900" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG30800 = RT38 2.5 gon W
        cs = fac.getCSByName( "EPSG:30800" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG31275 = MGI / Balkans zone 5
        cs = fac.getCSByName( "EPSG:31275" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG31276 = MGI / Balkans zone 6
        cs = fac.getCSByName( "EPSG:31276" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG31277 = MGI / Balkans zone 7
        cs = fac.getCSByName( "EPSG:31277" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG31278 = MGI / Balkans zone 8
        cs = fac.getCSByName( "EPSG:31278" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG31281 = MGI (Ferro) / Austria West Zone
        cs = fac.getCSByName( "EPSG:31281" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG31282 = MGI (Ferro) / Austria West Zone
        cs = fac.getCSByName( "EPSG:31282" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG31283 = MGI (Ferro) / Austria West Zone
        cs = fac.getCSByName( "EPSG:31283" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG31284 = MGI / M28
        cs = fac.getCSByName( "EPSG:31284" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG31285 = MGI / M31
        cs = fac.getCSByName( "EPSG:31285" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG31286 = MGI / M34
        cs = fac.getCSByName( "EPSG:31286" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23028 = ED50 / UTM zone 28N
        cs = fac.getCSByName( "EPSG:23028" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23029 = ED50 / UTM zone 29N
        cs = fac.getCSByName( "EPSG:23029" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23030 = ED50 / UTM zone 30N
        cs = fac.getCSByName( "EPSG:23030" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23031 = ED50 / UTM zone 31N
        cs = fac.getCSByName( "EPSG:23031" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23032 = ED50 / UTM zone 32N
        cs = fac.getCSByName( "EPSG:23032" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23033 = ED50 / UTM zone 33N
        cs = fac.getCSByName( "EPSG:23033" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23034 = ED50 / UTM zone 34N
        cs = fac.getCSByName( "EPSG:23034" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23035 = ED50 / UTM zone 35N
        cs = fac.getCSByName( "EPSG:23035" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23036 = ED50 / UTM zone 36N
        cs = fac.getCSByName( "EPSG:23036" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23037 = ED50 / UTM zone 37N
        cs = fac.getCSByName( "EPSG:23037" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23038 = ED50 / UTM zone 38N
        cs = fac.getCSByName( "EPSG:23038" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23090 = ED50 / TM 0 N
        cs = fac.getCSByName( "EPSG:23090" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG23095 = ED50 / TM 5 NE
        cs = fac.getCSByName( "EPSG:23095" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // NATIONAL DATUM of LUXEMBURG   LuRef
        cs = fac.getCSByName( "LuRef" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // cs = fac.getCSByName( "EPSG:32201 = WGS 72 / UTM zone 1N ... EPSG32260 = WGS 72 / UTM zone 60N )
        for ( int i = 1; i < 10; i++ ) {
            cs = fac.getCSByName( "EPSG:3220"+ i );
            systems.put( cs.getName( Locale.ENGLISH ),  cs );    
        }
        for ( int i = 10; i < 60; i++ ) {
            cs = fac.getCSByName( "EPSG:322"+ i );
            systems.put( cs.getName( Locale.ENGLISH ),  cs );
        }

        // EPSG32401 = WGS 72BE / UTM zone 1N ...EPSG32460 = WGS 72BE / UTM zone 60N
        for ( int i = 1; i < 10; i++ ) {
            cs = fac.getCSByName( "EPSG:3240" + i );
            systems.put( cs.getName( Locale.ENGLISH ),  cs );
        }
        for ( int i = 10; i < 60; i++ ) {
            cs = fac.getCSByName( "EPSG:324" + i );
            systems.put( cs.getName( Locale.ENGLISH ),  cs );
        }

        // EPSG32601 = WGS 84 / UTM zone 1N ...EPSG32660 = WGS 84 / UTM zone 60N
        for ( int i = 1; i < 10; i++ ) {
            cs = fac.getCSByName( "EPSG:3260" + i );
            systems.put( cs.getName( Locale.ENGLISH ),  cs );
        }
        for ( int i = 10; i < 60; i++ ) {
            cs = fac.getCSByName( "EPSG:326" + i );
            systems.put( cs.getName( Locale.ENGLISH ),  cs );
        }

        // EPSG32661 = WGS 84 / UPS North
        cs = fac.getCSByName( "EPSG:32661" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );

        // EPSG25828 = ETRS89 / UTM zone 28N ...EPSG25838 = ETRS89 / UTM zone 38N
        for ( int i = 28; i < 38; i++ ) {
            cs = fac.getCSByName( "EPSG:258" + i );
            systems.put( cs.getName( Locale.ENGLISH ),  cs );
        }

        // EPSG25884 = ETRS89 / TM Baltic93
        cs = fac.getCSByName( "EPSG:25884" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        // EPSG31466...EPSG31469
        for ( int i = 6; i < 10; i++ ) {
            cs = fac.getCSByName( "EPSG:3146" + i );
            systems.put( cs.getName( Locale.ENGLISH ),  cs );    
        }
        
        cs = fac.getCSByName( "EPSG:31287" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:31300" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        for (int i = 1; i < 6; i++) {
            cs = fac.getCSByName( "EPSG:2758" + 1 );
            systems.put( cs.getName( Locale.ENGLISH ),  cs );    
        }
        
        cs = fac.getCSByName( "EPSG:27291" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:27292" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
        cs = fac.getCSByName( "EPSG:27200" );
        systems.put( cs.getName( Locale.ENGLISH ),  cs );
        
    }
    
    /**
     *
     *
     * @return 
     */
    public String[] getKnownCS() {
        String[] array = new String[systems.size()];        
        return (String[])systems.keySet().toArray( array );
    }

    /**
     *
     *
     * @param name 
     *
     * @return 
     */
    public CoordinateSystem getCSByName( String name ) {
        return (CoordinateSystem)systems.get( name );
    }

    
    public static void main(String[] args) {
        ConvenienceCSFactoryFull csf = new ConvenienceCSFactoryFull();
        String[] kc = csf.getKnownCS();
        for (int i = 0; i < kc.length; i++) {
            System.out.println( kc[i] );
        } 
    }

    
}
