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

package org.deegree_impl.services.wts.util;

import java.util.*;

/**
 *
 * @author  Andreas Poth
 */
public class SunPosition {
    
    /**
     * calculates the solar altitude for given latitue, year, month, date, hour 
     * and minute
     * @param latitue latitue of the the viewers position
     * @param year year of the observation (leap year?)
     * @param month 1..12!
     * @param hour 0..23
     * @param minute 0..59
     */
    public static synchronized double calcVerticalSunposition(double latitue, 
                                         int year, int month, int date, int hour, 
                                         int minute)
    {
        // Hour Angle (H), 
        // Solar Declination (D),
        // Latitude (L)
        // solar altitude (A).
        // sin(A) = sin(D)*sin(L) + cos(D)*cos(L)*cos(H)
        double rad23_5 = Math.toRadians( 23.5 );
        double days = getDaySinceVernalEquinox( year, month, date );
        double sinD = Math.sin( rad23_5 ) * Math.sin( Math.toRadians(  days * 360.0 / 365.0 ) );
        double cosD = Math.cos( Math.asin( sinD ) );
        // the sun hour angle is zero when the object is on the meridian 
        double h = calcHorizontalSunPosition(hour, minute) - Math.toRadians(180);       
        double radL = Math.toRadians( latitue );
        double sinA = sinD * Math.sin(radL) + cosD * Math.cos(radL) * Math.cos(h);

        return Math.asin( sinA );
    }
    
    /**
     * calculates the horizontal angle of the sun depending only on hour and minute!
     * the date isn't required.
     * @param hour 0..23
     * @param minute 0..59
     */
    public static synchronized double calcHorizontalSunPosition(int hour, int minute)
    {
        double d = hour + minute/60.0;
        d = 180 + (( d - 12 ) * 15.0);
        return Math.toRadians(d);
    }
    
    /**
     * caluculates for a given date the number of days since the last vernal
     * equinox. (leap years are not considered yet)
     */
    private static double getDaySinceVernalEquinox(int year, int month, int date)
    {
        GregorianCalendar calendar = new GregorianCalendar( year, 2, 21 );
        int vEq = calendar.get( Calendar.DAY_OF_YEAR );
        calendar = new GregorianCalendar( year, month-1, date );
        int doy = calendar.get( Calendar.DAY_OF_YEAR );
        if ( doy < vEq ) {
            doy = ( 365 - vEq ) + doy;
        }         
        return doy-vEq;
    }
    
}
