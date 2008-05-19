/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.model.wspm.core.profil.changes;

/**
 * @author belger
 */
public class ProfilChangeHint
{
  private boolean m_objectChanged = false;

  private boolean m_objectDataChanged = false;

  private boolean m_pointValuesChanged = false;

  private boolean m_pointPropertiesChanged = false;

  private boolean m_pointsChanged = false;

  private boolean m_markerDataChanged = false;

  private boolean m_markerMoved = false;

  private boolean m_ProfilPropertyChanged = false;

  private boolean m_activePointChanged = false;

  public void setObjectChanged( )
  {
    m_objectChanged = true;
  }

  /** true, if building was added or removed or replaced */
  public boolean isObjectChanged( )
  {
    return m_objectChanged;
  }

  public void setObjectDataChanged( )
  {
    m_objectDataChanged = true;
  }

  /**
   * true, if data of the building was changed
   */
  public boolean isObjectDataChanged( )
  {
    return m_objectDataChanged;
  }

  public void setPointValuesChanged( )
  {
    m_pointValuesChanged = true;
  }

  /**
   * true, if values of one ore more point were changed
   */
  public boolean isPointValuesChanged( )
  {
    return m_pointValuesChanged;
  }

  public void setPointPropertiesChanged( )
  {
    m_pointPropertiesChanged = true;
  }

  /**
   * true, if pointProperty was remove or added
   */
  public boolean isPointPropertiesChanged( )
  {
    return m_pointPropertiesChanged;
  }

  public void setPointsChanged( )
  {
    m_pointsChanged = true;
  }

  /**
   * true if points were added or removed
   */
  public boolean isPointsChanged( )
  {
    return m_pointsChanged;
  }

  public void setMarkerMoved( )
  {
    m_markerMoved = true;
  }

  /**
   * true if one or more devider moved
   */
  public boolean isMarkerMoved( )
  {
    return m_markerMoved;
  }

  public void setMarkerDataChanged( )
  {
    m_markerDataChanged = true;
  }

  /**
   * true if one or more devider changed properties
   */
  public boolean isMarkerDataChanged( )
  {
    return m_markerDataChanged;
  }

  /**
   * true if profilPropertyChanged.
   */
  public boolean isProfilPropertyChanged( )
  {
    return m_ProfilPropertyChanged;
  }

  public void setProfilPropertyChanged( final boolean profilPropertyChanged )
  {
    m_ProfilPropertyChanged = profilPropertyChanged;
  }

  public void setActivePointChanged( )
  {
    m_activePointChanged = true;
  }

  /** true, if the active point or the active property of the profil changes. 
   * @return */
  public boolean isActivePointChanged( )
  {
    return m_activePointChanged;
  }
}
