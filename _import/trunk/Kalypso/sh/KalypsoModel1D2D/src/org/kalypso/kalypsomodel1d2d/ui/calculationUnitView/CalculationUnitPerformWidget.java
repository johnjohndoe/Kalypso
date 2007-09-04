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
package org.kalypso.kalypsomodel1d2d.ui.calculationUnitView;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.IGrabDistanceProvider;
import org.kalypso.kalypsomodel1d2d.ui.map.IWidgetWithStrategy;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.merge.Model1d2dCalUnitTheme;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

import de.renew.workflow.connector.cases.CaseHandlingSourceProvider;
import de.renew.workflow.connector.cases.ICaseDataProvider;

/**
 * @author Madanagopal
 * 
 */
public class CalculationUnitPerformWidget implements IWidgetWithOptions, IWidget, IWidgetWithStrategy, IGrabDistanceProvider
{
  private final CalculationUnitDataModel dataModel = new CalculationUnitDataModel();

  private final CalculationUnitPerformWidgetFace calcWidgetFace = new CalculationUnitPerformWidgetFace( dataModel );

  private final String m_name;

  private final String m_toolTip;

  private IWidget strategy;

  private Model1d2dCalUnitTheme m_calcUnitTheme;

  /**
   * The constructor.
   */

  public CalculationUnitPerformWidget( )
  {
    this( Messages.getString("CalculationUnitPerformWidget.0"), Messages.getString("CalculationUnitPerformWidget.1") ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public CalculationUnitPerformWidget( final String name, final String toolTip )
  {
    m_name = name;
    m_toolTip = toolTip;
  }

  private final KeyBasedDataModelChangeListener calThemeUpdater = new KeyBasedDataModelChangeListener()
  {
    public void dataChanged( String key, Object newValue )
    {
      if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) )
      {
        m_calcUnitTheme.setCalculationUnit( (ICalculationUnit) newValue );
        KeyBasedDataModelUtil.repaintMapPanel( dataModel, ICommonKeys.KEY_MAP_PANEL );
      }
    }

  };

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#createControl(org.eclipse.swt.widgets.Composite,
   *      org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    try
    {
      dataModel.setData( ICommonKeys.KEY_SELECTED_DISPLAY, parent.getDisplay() );
      return calcWidgetFace.createControl( parent );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      return null;
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions#disposeControl()
   */
  public void disposeControl( )
  {
    if( calcWidgetFace != null )
    {
      calcWidgetFace.disposeControl();
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    dataModel.setData( ICommonKeys.KEY_MAP_PANEL, mapPanel );
    final IMapModell mapModell = mapPanel.getMapModell();

    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final ICaseDataProvider<IFeatureWrapper2> modelProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( CaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    IFEDiscretisationModel1d2d m_model;
    try
    {
      m_model = modelProvider.getModel( IFEDiscretisationModel1d2d.class );
      // final IFEDiscretisationModel1d2d m_model = UtilMap.findFEModelTheme( mapModell );
      dataModel.setData( ICommonKeys.KEY_DISCRETISATION_MODEL, m_model );
      dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, CalcUnitOps.getModelCalculationUnits( m_model ) );
      dataModel.setData( ICommonKeys.WIDGET_WITH_STRATEGY, this );

      // TODO: what is the purpose of this extra theme??! This is completely senseless!
      m_calcUnitTheme = new Model1d2dCalUnitTheme( Messages.getString("CalculationUnitPerformWidget.2"), mapModell ); //$NON-NLS-1$
      mapModell.insertTheme( m_calcUnitTheme, 0 );
      dataModel.addKeyBasedDataChangeListener( calThemeUpdater );

      // command manager since it is use in the dirty pool object framework
      // the commandable workspace of the target theme is taken
      // TODO: that cannot work, as the models workspace is not a commandable workspace
      dataModel.setData( ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL, m_model.getWrappedFeature().getWorkspace() );

      dataModel.setData( ICommonKeys.KEY_GRAB_DISTANCE_PROVIDER, this );
      dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, CalcUnitOps.getModelCalculationUnits( m_model ) );

      final IKalypsoFeatureTheme operationalTheme = UtilMap.findEditableTheme( mapModell, IBoundaryCondition.QNAME );
      dataModel.setData( ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE, operationalTheme.getWorkspace() );

      final IFlowRelationshipModel bcModel = Util.getModel( IFlowRelationshipModel.class );// (IFlowRelationshipModel)

      m_calcUnitTheme.setModelBoundaryConditions( bcModel );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#canBeActivated(org.eclipse.jface.viewers.ISelection,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  public boolean canBeActivated( final ISelection selection, final MapPanel mapPanel )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( final Point p )
  {
    if( strategy != null )
    {
      strategy.clickPopup( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedLeft(java.awt.Point)
   */
  public void doubleClickedLeft( final Point p )
  {
    if( strategy != null )
    {
      strategy.doubleClickedLeft( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#doubleClickedRight(java.awt.Point)
   */
  public void doubleClickedRight( final Point p )
  {
    if( strategy != null )
    {
      strategy.doubleClickedRight( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( final Point p )
  {
    if( strategy != null )
    {
      strategy.dragged( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish( )
  {
    try
    {
      final MapPanel mapPanel = (MapPanel) dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
      final IMapModell mapModell = mapPanel.getMapModell();
      if( mapModell != null )
      {
        mapModell.removeTheme( m_calcUnitTheme );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    try
    {
      if( strategy != null )
      {
        strategy.finish();
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getName()
   */
  public String getName( )
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#getToolTip()
   */
  public String getToolTip( )
  {
    return m_toolTip;
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyPressed(java.awt.event.KeyEvent)
   */
  public void keyPressed( final KeyEvent e )
  {
    if( strategy != null )
    {
      strategy.keyPressed( e );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyReleased(java.awt.event.KeyEvent)
   */
  public void keyReleased( final KeyEvent e )
  {
    if( strategy != null )
    {
      strategy.keyReleased( e );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#keyTyped(java.awt.event.KeyEvent)
   */
  public void keyTyped( final KeyEvent e )
  {
    if( strategy != null )
    {
      strategy.keyTyped( e );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( final Point p )
  {
    if( strategy != null )
    {
      strategy.leftClicked( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( final Point p )
  {
    if( strategy != null )
    {
      strategy.leftPressed( p );
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( final Point p )
  {
    if( strategy != null )
    {
      strategy.leftReleased( p );
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( final Point p )
  {
    if( strategy != null )
    {
      strategy.middleClicked( p );
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( final Point p )
  {
    if( strategy != null )
    {
      strategy.middlePressed( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( final Point p )
  {
    if( strategy != null )
    {
      strategy.middleReleased( p );
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( final Point p )
  {
    if( strategy != null )
    {
      strategy.moved( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( final Graphics g )
  {
    if( strategy != null )
    {
      strategy.paint( g );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( final Point p )
  {
    if( strategy != null )
    {
      strategy.rightClicked( p );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( final Point p )
  {
    if( strategy != null )
    {
      strategy.rightPressed( p );
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( final Point p )
  {
    if( strategy != null )
    {
      strategy.rightReleased( p );
    }

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#setSelection(org.eclipse.jface.viewers.ISelection)
   */
  public void setSelection( final ISelection selection )
  {
    if( strategy != null )
    {
      strategy.setSelection( selection );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.IWidgetWithStrategy#setStrategy(org.kalypso.ogc.gml.widgets.IWidget)
   */
  public void setStrategy( final IWidget strategy )
  {
    if( this.strategy != null )
    {
      this.strategy.finish();
    }

    this.strategy = strategy;
    if( this.strategy != null )
    {
      // TODO: this is probably no talways the right command targert
      final ICommandTarget commandPoster = (ICommandTarget) dataModel.getData( ICommonKeys.KEY_COMMAND_TARGET_DISC_MODEL );
      final MapPanel mapPanel = (MapPanel) dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
      this.strategy.activate( commandPoster, mapPanel );
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.IGrabDistanceProvider#getGrabDistance()
   */
  public double getGrabDistance( )
  {
    if( strategy instanceof IGrabDistanceProvider )
    {
      return ((IGrabDistanceProvider) strategy).getGrabDistance();
    }

    System.out.println( "getting fix grab distance" ); //$NON-NLS-1$

    final MapPanel mapPanel = dataModel.getData( MapPanel.class, ICommonKeys.KEY_MAP_PANEL );
    return MapUtilities.calculateWorldDistance( mapPanel, 6 );
  }

}
