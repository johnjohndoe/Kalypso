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

 copyright Emanuele Tajariol / itworks
                 
 ---------------------------------------------------------------------------*/

package org.deegree.services;

import java.util.*;


/**
 * TimeExtent implements the ISO extended time format
 * The syntax for expressing Time constraints and date / time values is
 * specified in WMS 1.1 Annexes B
 */
public interface TimeExtent extends Extent {
  
    /** Used for single valued time extents*/
    public Calendar getDate();
  
    /** Used for listed time extents*/
    public Calendar getDate( int i );

    /** Used for periodic time extents*/
    public TimePeriod getPeriod();

    /** Used for periodic time extents*/
    public Calendar getStartDate();

    /** Used for periodic time extents*/
    public Calendar getEndDate();

    /**
     *      
     */
    public class TimePeriod {
        public static final int YEAR = 0;
        public static final int MONTH = 1;
        public static final int DAY = 2;
        public static final int HOUR = 3;
        public static final int MINUTE = 4;
        public static final int SECOND = 5;
        private boolean _decadic = false;

        private int d;
        private int h;
        private int mi;
        private int mo;
        private int s;
        private int y;

        /**
         * Creates a new TimePeriod object.
         *
         * @param isoPeriod 
         */
        public TimePeriod( String isoPeriod ) {
            init();

            if ( isoPeriod.equalsIgnoreCase( "decadic" ) ) // not standard ISO!
            {
                _decadic = true;
                return;
            }
            //TODO parse isoperiod
        }

        /**
         *
         */
        private void init() {
            y = -1;
            mo = -1;
            d = -1;
            h = -1;
            mi = -1;
            s = -1;
        }

        /**
         *
         *
         * @return 
         */
        public boolean isDecadic() {
            return _decadic;
        }

        /**
         *
         *
         * @param field 
         *
         * @return 
         */
        public int get( int field ) {
            switch ( field ) {
                case YEAR:
                    return y;
                case MONTH:
                    return mo;
                case DAY:
                    return d;
                case HOUR:
                    return h;
                case MINUTE:
                    return mi;
                case SECOND:
                    return s;
                default:
                    return -1;
            }
        }

        /**
         *
         *
         * @param field 
         *
         * @return 
         */
        public boolean isDefined( int field ) {
            switch ( field ) {
                case YEAR:
                    return y != -1;
                case MONTH:
                    return mo != -1;
                case DAY:
                    return d != -1;
                case HOUR:
                    return h != -1;
                case MINUTE:
                    return mi != -1;
                case SECOND:
                    return s != -1;
                default:
                    return false;
            }
        }
    }
}