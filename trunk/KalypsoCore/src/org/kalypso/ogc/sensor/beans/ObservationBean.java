/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.beans;

import java.util.Map;

import org.kalypso.repository.beans.ItemBean;

/**
 * An Observation Bean. Specialisation of <code>ItemBean</code>. Can deliver the MetadataList
 * for the IObservation it represents.
 * 
 * @author schlienger
 */
public class ObservationBean extends ItemBean
{
  private Map m_metadataList;

  public ObservationBean()
  {
    this( "", "", "", null );
  }
  
  public ObservationBean( final String id )
  {
    this( id, "", "", null );
  }
  
  public ObservationBean( final String id, final String name, final String repId, final Map metadata )
  {
    super( id, name, repId );
    
    m_metadataList = metadata;
  }
  
  public Map getMetadataList()
  {
    return m_metadataList;
  }
  
  public void setMetadataList( final Map md )
  {
    m_metadataList = md;
  }
}