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
package org.kalypso.ogc.gml.map;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

/**
 * @author Stefan Kurzbach
 */
public class BaseMapSchedulingRule implements IMapSchedulingRule
{
  private final MapPanel m_mapPanel;

  private final IResource m_mapFile;

  private ActivateLayerSchedulingRule m_activateLayerSchedulingRule;

  private SelectWidgetSchedulingRule m_selectWidgetSchedulingRule;

  public BaseMapSchedulingRule( final MapPanel mapPanel, final IResource mapFile )
  {
    m_mapPanel = mapPanel;
    m_mapFile = mapFile;
  }

  public ActivateLayerSchedulingRule getActivateLayerSchedulingRule( )
  {
    if( m_activateLayerSchedulingRule == null )
    {
      m_activateLayerSchedulingRule = new ActivateLayerSchedulingRule( this );
    }
    return m_activateLayerSchedulingRule;
  }

  public SelectWidgetSchedulingRule getSelectWidgetSchedulingRule( )
  {
    if( m_selectWidgetSchedulingRule == null )
    {
      m_selectWidgetSchedulingRule = new SelectWidgetSchedulingRule( this );
    }
    return m_selectWidgetSchedulingRule;
  }

  public MapPanel getMapPanel( )
  {
    return m_mapPanel;
  }

  public IResource getMapFile( )
  {
    return m_mapFile;
  }

  /**
   * @see org.eclipse.core.runtime.jobs.ISchedulingRule#contains(org.eclipse.core.runtime.jobs.ISchedulingRule)
   */
  public boolean contains( final ISchedulingRule rule )
  {
    if( this == rule )
    {
      return true;
    }
    if( rule instanceof IMapSchedulingRule )
    {
      // contains all other instances of IMapSchedulingRule with the same map panel
      IMapSchedulingRule other = (IMapSchedulingRule) rule;
      return getMapPanel() == other.getMapPanel();
    }
    else if( rule instanceof IResource )
    {
      // contains the underlying file
      final IResource other = (IResource) rule;
      return other.equals( getMapFile() );
    }
    return false;
  }

  /**
   * @see org.eclipse.core.runtime.jobs.ISchedulingRule#isConflicting(org.eclipse.core.runtime.jobs.ISchedulingRule)
   */
  public boolean isConflicting( final ISchedulingRule rule )
  {
    if( rule instanceof IMapSchedulingRule )
    {
      // conflicts with all other instances of IMapSchedulingRule with the same map panel
      final IMapSchedulingRule other = (IMapSchedulingRule) rule;
      return getMapPanel() == other.getMapPanel();
    }
    else if( rule instanceof IResource )
    {
      // conflicts with the underlying file
      final IResource other = (IResource) rule;
      return other.equals( m_mapFile );
    }
    else
      return false;
  }

  private class ActivateLayerSchedulingRule implements IMapSchedulingRule
  {
    private final BaseMapSchedulingRule m_parent;

    public ActivateLayerSchedulingRule( final BaseMapSchedulingRule parent )
    {
      m_parent = parent;
    }

    public MapPanel getMapPanel( )
    {
      return m_parent.getMapPanel();
    }

    /**
     * @see org.eclipse.core.runtime.jobs.ISchedulingRule#contains(org.eclipse.core.runtime.jobs.ISchedulingRule)
     */
    public boolean contains( final ISchedulingRule rule )
    {
      if( this == rule )
      {
        return true;
      }
      if( rule instanceof ActivateLayerSchedulingRule )
      {
        // contains only instances of ActivateLayerSchedulingRule with the same map panel
        ActivateLayerSchedulingRule other = (ActivateLayerSchedulingRule) rule;
        return getMapPanel() == other.getMapPanel();
      }
      return false;
    }

    /**
     * @see org.eclipse.core.runtime.jobs.ISchedulingRule#isConflicting(org.eclipse.core.runtime.jobs.ISchedulingRule)
     */
    public boolean isConflicting( final ISchedulingRule rule )
    {
      if( rule instanceof IMapSchedulingRule )
      {
        // conflicts with all other instances of IMapSchedulingRule with the same map panel
        final IMapSchedulingRule other = (IMapSchedulingRule) rule;
        return getMapPanel() == other.getMapPanel();
      }
      return false;
    }
  }

  private class SelectWidgetSchedulingRule implements IMapSchedulingRule
  {
    private final BaseMapSchedulingRule m_parent;

    public SelectWidgetSchedulingRule( final BaseMapSchedulingRule parent )
    {
      m_parent = parent;
    }

    public MapPanel getMapPanel( )
    {
      return m_parent.getMapPanel();
    }

    /**
     * @see org.eclipse.core.runtime.jobs.ISchedulingRule#contains(org.eclipse.core.runtime.jobs.ISchedulingRule)
     */
    public boolean contains( final ISchedulingRule rule )
    {
      if( this == rule )
      {
        return true;
      }
      if( rule instanceof SelectWidgetSchedulingRule )
      {
        // contains only instances of SelectWidgetSchedulingRule with the same map panel
        SelectWidgetSchedulingRule other = (SelectWidgetSchedulingRule) rule;
        return getMapPanel() == other.getMapPanel();
      }
      return false;
    }

    /**
     * @see org.eclipse.core.runtime.jobs.ISchedulingRule#isConflicting(org.eclipse.core.runtime.jobs.ISchedulingRule)
     */
    public boolean isConflicting( final ISchedulingRule rule )
    {
      if( rule instanceof IMapSchedulingRule )
      {
        // conflicts with all other instances of IMapSchedulingRule with the same map panel
        final IMapSchedulingRule other = (IMapSchedulingRule) rule;
        return getMapPanel() == other.getMapPanel();
      }
      return false;
    }
  }
}
