/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.convert.namodel.manager;

import java.util.Iterator;
import java.util.List;

import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author huebsch
 */
public class SchneeManager
{
  private final ASCIIHelper m_asciiHelper;

  public SchneeManager( )
  {
    m_asciiHelper = new ASCIIHelper( getClass().getResource( "resources/formats/parameter.txt" ) ); //$NON-NLS-1$
  }

  public void writeFile( final StringBuffer snowBuffer, final GMLWorkspace paraWorkspace ) throws Exception
  {
    final Feature rootFeature = paraWorkspace.getRootFeature();
    final List< ? > list = (List< ? >) rootFeature.getProperty( NaModelConstants.PARA_PROP_SNOW_MEMBER );
    snowBuffer.append( Messages.getString( "org.kalypso.convert.namodel.manager.SchneeManager.0" ) ); //$NON-NLS-1$
    snowBuffer.append( "/                     wwo wwmax snotem snorad h0\n" ); //$NON-NLS-1$
    snowBuffer.append( "/                      *    *     *      *    *\n" ); //$NON-NLS-1$
    final Iterator< ? > iter = list.iterator();
    while( iter.hasNext() )
    {
      final Feature snowFE = (Feature) iter.next();
      // TODO: nur die schreiben, die auch in Gebietsdatei vorkommen
      writeFeature( snowBuffer, snowFE );
    }

  }

  private void writeFeature( final StringBuffer snowBuffer, final Feature feature ) throws Exception
  {
    snowBuffer.append( m_asciiHelper.toAscii( feature, 13 ) + "\n" ); //$NON-NLS-1$
  }

}
