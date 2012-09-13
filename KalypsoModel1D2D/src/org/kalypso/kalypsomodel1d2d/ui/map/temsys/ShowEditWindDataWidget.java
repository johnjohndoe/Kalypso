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

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.Arrays;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.deegree.framework.util.Pair;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModel;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataModelSystem;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindDataProvider;
import org.kalypso.kalypsosimulationmodel.core.wind.IWindModel;
import org.kalypso.kalypsosimulationmodel.core.wind.NativeWindDataModelHelper;
import org.kalypso.kalypsosimulationmodel.core.wind.NativeWindDataModelWrapper;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypso.ogc.gml.IKalypsoCascadingTheme;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget;
import org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.action.AddThemeCommand;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 *
 * @author ig
 *
 */
public class ShowEditWindDataWidget extends AbstractDelegateWidget implements IWidgetWithOptions
{
  private static final String FENET_CONTEXT = "fenet"; //$NON-NLS-1$

  private static final String WIND_GML_SOURCE_FILE = "../models/wind.gml"; //$NON-NLS-1$

  private static final String FEATURE_PATH_WIND_DATA_MODEL = "#fid#root/windDataModelSystem"; //$NON-NLS-1$

  private static final String GML_TYPE_STR = "gml"; //$NON-NLS-1$

  private static final String WIND_THEME_NAME = "Wind"; //$NON-NLS-1$

  private final WindDataWidgetDataModel m_dataModel = new WindDataWidgetDataModel();

  private final WindDataShowWidgetFace m_widgetFace = new WindDataShowWidgetFace( m_dataModel );

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final ToolTipRenderer m_toolTipRendererDesc = new ToolTipRenderer();

  private Point m_point;

  private final SelectFeatureWidget m_selDelegateWidget;

  public ShowEditWindDataWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ShowEditWindDataWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ShowEditWindDataWidget.1" ), new SelectFeatureWidget( StringUtils.EMPTY, StringUtils.EMPTY, new QName[] { NativeWindDataModelWrapper.SIM_BASE_F_NATIVE_WIND_ELE_WRAPPER, IWindDataModelSystem.SIM_BASE_F_WIND_ELE_SYS, KalypsoModelSimulationBaseConsts.SIM_BASE_F_BASE_WIND_ELE_MODEL }, IFE1D2DNode.PROPERTY_POINT ) ); //$NON-NLS-1$ //$NON-NLS-2$

    m_toolTipRendererDesc.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ShowEditWindDataWidget.2" ) );//$NON-NLS-1$
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );

    m_selDelegateWidget = (SelectFeatureWidget) getDelegate();
  }

  private void checkWindTheme( final ICommandTarget pCommandPoster )
  {
    final IMapPanel mapPanel = getMapPanel();//outlineView.getMapPanel();
    final IKalypsoTheme lTheme = findTheme( mapPanel, WIND_THEME_NAME );
    if( lTheme != null ){
      return;
    }
    final IKalypsoLayerModell mapModell = mapPanel.getMapModell();

    final AddThemeCommand command = new AddThemeCommand( mapModell, WIND_THEME_NAME, GML_TYPE_STR, FEATURE_PATH_WIND_DATA_MODEL, WIND_GML_SOURCE_FILE ); //$NON-NLS-1$
    pCommandPoster.postCommand( command, null );

  }

  private IKalypsoTheme findTheme( final IMapPanel mapPanel, final String pThemeName )
  {
    try
    {
      final String lThemeName = pThemeName.toLowerCase();
      final List<IKalypsoTheme> themesAct = Arrays.asList( mapPanel.getMapModell().getAllThemes() );
      if( themesAct != null )
      {
        for( final IKalypsoTheme lTheme : themesAct )
        {
          final String lThemeContext = lTheme.getContext().toExternalForm().toLowerCase();
          if( !lThemeContext.contains( FENET_CONTEXT ) ){
            continue;
          }
          if( lTheme instanceof IKalypsoCascadingTheme )
          {
            final IKalypsoCascadingTheme lThemes = (IKalypsoCascadingTheme) lTheme;
            for( int i = 0; i < lThemes.getAllThemes().length; i++ )
            {
              try
              {
                if( lTheme.getName().getKey().toLowerCase().contains( lThemeName ) )
                {
                  return lTheme;
                }
              }
              catch( final Exception e )
              {
              }
            }
          }
          else
          {
            if( lTheme.getName().getKey().toLowerCase().contains( lThemeName ) )
            {
              return lTheme;
            }
          }
        }
      }
    }
    catch( final Exception e )
    {

    }
    return null;
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    checkWindTheme( commandPoster );
    /* set data to data model */
    if( mapPanel == null )
      return;

    m_dataModel.setMapPanel( mapPanel );

    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return;

    m_dataModel.setMapModell( mapModell );

    final IKalypsoTheme[] themes = mapModell.getAllThemes();
    for( final IKalypsoTheme theme : themes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme ft = (IKalypsoFeatureTheme) theme;
        final IFeatureType ftp = ft.getFeatureType();
        final QName qName = ftp == null ? null : ftp.getQName();
        if( IWindDataModelSystem.SIM_BASE_F_WIND_ELE_SYS.equals( qName ) )
        {
          final IKalypsoFeatureTheme[] fts = new IKalypsoFeatureTheme[1];
          fts[0] = ft;
          m_selDelegateWidget.setThemes( fts );
        }
      }
    }

    // find and set wind model system
    final IKalypsoFeatureTheme lWindDataTheme = UtilMap.findEditableTheme( mapPanel, IWindDataModelSystem.SIM_BASE_F_WIND_ELE_SYS );
    // IKalypsoFeatureTheme lWindDataTheme = UtilMap.findEditableTheme( mapPanel,
    // KalypsoModelSimulationBaseConsts.SIM_BASE_F_BASE_WIND_ELE_MODEL );
    if( lWindDataTheme != null )
    {
      final Feature lWindSystemFeature = lWindDataTheme.getFeatureList().getOwner();
      final IWindModel lWindModel = (IWindModel) lWindSystemFeature.getAdapter( IWindModel.class );
      if( lWindModel.getWindDataModelSystems().size() > 0 )
      {
        final IWindDataModelSystem lWindSystem = lWindModel.getWindDataModelSystems().get( 0 );// .getAdapter(
        // IWindDataModelSystem.class
        // );
        m_dataModel.setWindDataModelSystem( lWindSystem );

      }
      m_dataModel.setData( IWindModel.class.toString(), lWindModel );
      m_dataModel.setWindTheme( lWindDataTheme );
    }

  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    return m_widgetFace.createControl( parent );
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  @Override
  public void disposeControl( )
  {
    if( m_widgetFace != null )
      m_widgetFace.disposeControl();

    m_dataModel.removeAllListeners();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.ITerrainModelConsumer#setTerrainModel(org.kalypso.kalypsosimulationmodel.core.terrainmodel.IWindDataModel)
   */
  public void setWindModel( final IWindDataModel pWindModel )
  {
    m_dataModel.setWindDataModel( pWindModel );
  }

  @Override
  public void moved( final Point p )
  {
    try
    {
      super.moved( p );
    }
    catch( final Exception e )
    {
      return;
    }

    m_point = p;

    final IMapPanel mapPanel = m_dataModel.getMapPanel();
    if( mapPanel != null )
      mapPanel.repaintMap();
  }

  private final void paintWindDataTooltip( final Graphics g, final Point p )
  {
    if( m_widgetFace.isAnimating() )
    {

      return;
    }
    final Color color = g.getColor();
    g.setColor( Color.BLACK );
    try
    {
      if( p == null )
        return;

      final IMapPanel mapPanel = m_dataModel.getMapPanel();
      if( mapPanel == null || mapPanel.getProjection() == null )
        return;

      final GM_Point nodePoint = MapUtilities.transform( mapPanel, p );

      final StringBuffer tooltipText = new StringBuffer();

      final IWindDataProvider windProvider = m_dataModel.getWindDataModel();
      if( windProvider != null && nodePoint != null )
      {
        final Pair<Double, Double> wind1 = windProvider.getWindAsVector( nodePoint );

        final Pair<Double, Double> wind = NativeWindDataModelHelper.convertVectorWindToSpeedAndDirection( wind1 );
        if( wind != null && !Double.isNaN( wind.first ) && !Double.isNaN( wind.second ) )
        {
          tooltipText.append( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ShowEditWindDataWidget.3" ) ); //$NON-NLS-1$
          tooltipText.append( String.format( ": %.3f m/s %.3f deg ", wind.first, wind.second ) ); //$NON-NLS-1$
          tooltipText.append( String.format( "; %.3f U %.3f V ", wind1.first, wind1.second ) ); //$NON-NLS-1$
        }
        else
          tooltipText.append( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ShowEditWindDataWidget.4" ) ); //$NON-NLS-1$
      }
      else
        tooltipText.append( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ShowEditWindDataWidget.4" ) ); //$NON-NLS-1$

      m_toolTipRenderer.setTooltip( tooltipText.toString() );
      m_toolTipRenderer.paintToolTip( p, g, getMapPanel().getScreenBounds() );

      return;
    }
    catch( final RuntimeException e )
    {
      e.printStackTrace();
    }
    finally
    {
      g.setColor( color );
    }
  }

  @Override
  public void doubleClickedLeft( final Point p )
  {
    super.doubleClickedLeft( p );
  }

  @Override
  public void paint( final Graphics g )
  {
    final Graphics2D g2 = (Graphics2D) g;

    super.paint( g2 );

    paintWindDataTooltip( g2, m_point );

    final IMapPanel mapPanel = m_dataModel.getMapPanel();
    if( mapPanel == null )
      return;

    final Rectangle bounds = mapPanel.getScreenBounds();
    final String delegateTooltip = getDelegate().getToolTip();

    if( m_widgetFace.isAnimating() )
    {
      m_toolTipRendererDesc.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ShowEditWindDataWidget.5" ) + " " + delegateTooltip ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    else
    {
      m_toolTipRendererDesc.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.temsys.ShowEditWindDataWidget.6" ) + " " + delegateTooltip ); //$NON-NLS-1$ //$NON-NLS-2$
    }
    m_toolTipRendererDesc.paintToolTip( new Point( 5, bounds.height - 5 ), g2, bounds );
  }

  @Override
  public synchronized boolean canBeActivated( final ISelection selection, final IMapPanel mapPanel )
  {
    return true;
  }

  @Override
  public String getPartName( )
  {
    return null;
  }
}