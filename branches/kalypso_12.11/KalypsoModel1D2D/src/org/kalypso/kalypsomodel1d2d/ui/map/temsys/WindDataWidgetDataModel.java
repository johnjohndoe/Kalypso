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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.ICommandPoster;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModel;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.geometry.GM_PolygonPatch;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author ig
 *
 */
public class WindDataWidgetDataModel extends KeyBasedDataModel
{
  public static final String STR_WIND_STEP_PROP_NAME = "windStepActual"; //$NON-NLS-1$

  public static final String WIND_THEME = "_WINDDATA_THEME_"; //$NON-NLS-1$

  private static final String[] KEYS = { IWindModel.class.toString(), IWindDataModelSystem.class.toString(), IWindDataModel.class.toString(), IMapModell.class.toString(), IMapPanel.class.toString(),
    WIND_THEME };

  private final IScenarioDataProvider m_dataProvider;

  public WindDataWidgetDataModel( )
  {
    super( KEYS, null );

    m_dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
  }

  public void setMapModell( final IMapModell mapModell )
  {
    setData( IMapModell.class.toString(), mapModell );
  }

  public IMapModell getMapModell( )
  {
    return (IMapModell) getData( IMapModell.class.toString() );
  }

  public CommandableWorkspace getWindModelWorkspace( )
  {
    try
    {
      return ((ICommandPoster) m_dataProvider).getCommandableWorkSpace( IWindModel.class.getName() );
    }
    catch( final IllegalArgumentException e )
    {
      e.printStackTrace();
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }
    return null;
  }

  public IWindDataModel getWindDataModel( )
  {
    return (IWindDataModel) getData( IWindDataModel.class.toString() );
  }

  public void setWindDataModel( final IWindDataModel pWindDataModel )
  {
    setData( IWindDataModel.class.toString(), pWindDataModel, true );
  }

  public IWindDataModelSystem getWindDataModelSystem( )
  {
    return (IWindDataModelSystem) getData( IWindDataModelSystem.class.toString() );
  }

  public void setWindDataModelSystem( final IWindDataModelSystem pWindDataModelSystem )
  {
    setData( IWindDataModelSystem.class.toString(), pWindDataModelSystem );
    if( pWindDataModelSystem != null )
    {
      setSelectedWindSystem( pWindDataModelSystem );
    }
  }

  public GM_PolygonPatch getSelectionArea( )
  {
    return (GM_PolygonPatch) getData( GM_PolygonPatch.class.toString() );
  }

  public void setSelectionArea( final GM_PolygonPatch selectionArea )
  {
    setData( GM_PolygonPatch.class.toString(), selectionArea );
  }

  public IKalypsoFeatureTheme getWindTheme( )
  {
    return (IKalypsoFeatureTheme) getData( WIND_THEME );
  }

  public void setWindTheme( final IKalypsoFeatureTheme windTheme )
  {
    setData( WIND_THEME, windTheme );
  }

  public IMapPanel getMapPanel( )
  {
    return (IMapPanel) getData( IMapPanel.class.toString() );
  }

  public void setMapPanel( final IMapPanel mapPanel )
  {
    setData( IMapPanel.class.toString(), mapPanel );
  }

  public final IFeatureBindingCollection<IWindDataModel> getWindDataModels( )
  {
    final IWindDataModelSystem lWindDataModelSystem = getWindDataModelSystem();
    if( lWindDataModelSystem == null )
      return null;
    else
      return lWindDataModelSystem.getWindDataModels();
  }

  private static Map<String, Map<String, Object>> m_mapProperiesData = new HashMap<>();

  private static IWindDataModelSystem m_selectedWindSystem;

  public static synchronized IWindDataModel getActualWindDataModel( final String pStrSystemId )
  {
    final Map<String, Object> lMapActualWindProps = m_mapProperiesData.get( pStrSystemId );
    if( lMapActualWindProps != null )
    {
      final IWindDataModel lActWindDataModel = (IWindDataModel) lMapActualWindProps.get( STR_WIND_STEP_PROP_NAME );
      if( lActWindDataModel != null )
      {
        return lActWindDataModel;
      }
    }
    return null;
  }

  // public static Map getActualCssMap( final String pStrWidgetId ){
  // Map<String, Object> lMapActualWindProps = m_mapProperiesData.get( pStrWidgetId );
  // if( lMapActualWindProps != null ){
  // return (Map) lMapActualWindProps.get( STR_WIND_VECTOR_CSS );
  // }
  // return null;
  // }
  //
  // public static void setActualCssMap( final String pStrWidgetId, final Map pMapActualWindProps ){
  // Map<String, Object> lMapActualWindProps = m_mapProperiesData.get( pStrWidgetId );
  // if( lMapActualWindProps == null ){
  // lMapActualWindProps = new HashMap<String, Object>();
  // m_mapProperiesData.put( pStrWidgetId, lMapActualWindProps );
  // }
  // lMapActualWindProps.put( STR_WIND_VECTOR_CSS, pMapActualWindProps );
  // }

  public static synchronized void setActualWindDataModel( final String pStrWindSystemId, final IWindDataModel pActualWindDataModel )
  {
    Map<String, Object> lMapActualWindProps = m_mapProperiesData.get( pStrWindSystemId );
    if( lMapActualWindProps == null )
    {
      lMapActualWindProps = new HashMap<>();
      m_mapProperiesData.put( pStrWindSystemId, lMapActualWindProps );
    }
    lMapActualWindProps.put( STR_WIND_STEP_PROP_NAME, pActualWindDataModel );
  }

  public static synchronized IWindDataModelSystem getSelectedWindSystem( )
  {
    return m_selectedWindSystem;
  }

  public static final void setSelectedWindSystem( final IWindDataModelSystem windSystem )
  {
    m_selectedWindSystem = windSystem;
  }

  public void saveModels( ) throws CoreException
  {
    m_dataProvider.saveModel( new NullProgressMonitor() );
  }
}
