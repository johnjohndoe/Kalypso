/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.convert.namodel.hydrotope;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.convert.namodel.NaModelConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Dirk Kuch
 */
public class LanduseClassHelper
{
  private final static String ID_FORMAT = "PLC_%05d";

  public static Map<String, String> resolve( final GMLWorkspace landuseClassesWorkspace )
  {
    final List< ? > landuseClassesFeatures = (List< ? >) landuseClassesWorkspace.getRootFeature().getProperty( new QName( NaModelConstants.NS_NAPARAMETER, "landuseMember" ) ); //$NON-NLS-1$
    int cnt = 1;

    final Map<String, String> landuseClasses = new HashMap<String, String>();
    for( final Object object : landuseClassesFeatures )
    {
      final Feature f = (Feature) object;
      final String name = f.getName();
      String id = formatID( f.getId(), cnt++ );
      
      // TODO check if this check is necessary (possible that some of the existing classes are called PLC_nnnnn)
      while( landuseClasses.containsValue( id ) )
        id = formatID( f.getId(), cnt++ );
      
      landuseClasses.put( name, id );
    }
    return landuseClasses;
  }

  private final static String formatID( final String id, final int nextCnt )
  {
    if( id.length() <= 10 )
      return id;
    return String.format( ID_FORMAT, nextCnt );
  }

}
