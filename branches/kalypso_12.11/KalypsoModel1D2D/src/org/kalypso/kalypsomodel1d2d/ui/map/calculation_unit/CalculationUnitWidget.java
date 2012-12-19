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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ui.map.merge.Model1d2dCalUnitTheme;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypsodeegree.model.feature.Feature;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Patrice Congo
 * @author Madanagopal
 */
public class CalculationUnitWidget extends AbstractDelegateWidget implements IWidgetWithOptions
{
  private final CalculationUnitDataModel m_dataModel = new CalculationUnitDataModel();

  private final CalculationUnitWidgetFace m_widgetFace = new CalculationUnitWidgetFace( m_dataModel );

  private final String m_name;

  private final String m_toolTip;

  private Model1d2dCalUnitTheme m_calcUnitTheme;

  private final KeyBasedDataModelChangeListener calThemeUpdater = new KeyBasedDataModelChangeListener()
  {
    @Override
    @SuppressWarnings("synthetic-access")
    public void dataChanged( final String key, final Object newValue )
    {
      if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) )
        m_calcUnitTheme.setCalculationUnit( (ICalculationUnit) newValue );
    }
  };

  public CalculationUnitWidget( )
  {
    this( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public CalculationUnitWidget( final String name, final String toolTip )
  {
    super( name, toolTip, null );

    m_name = name;
    m_toolTip = toolTip;
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    m_dataModel.setData( ICommonKeys.KEY_MAP_PANEL, mapPanel );
    m_dataModel.setData( ICommonKeys.KEY_COMMAND_TARGET_DISC_MODEL, commandPoster );
    final IMapModell mapModell = mapPanel.getMapModell();
    final IFEDiscretisationModel1d2d model1d2d = UtilMap.findFEModelTheme( mapPanel );

    final IScenarioDataProvider modelProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    m_dataModel.setData( ICommonKeys.KEY_DATA_PROVIDER, modelProvider );

    m_dataModel.setData( ICommonKeys.KEY_DISCRETISATION_MODEL, model1d2d );
    m_dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, CalcUnitOps.getModelCalculationUnits( model1d2d ) );
    m_dataModel.setData( ICommonKeys.WIDGET_WITH_STRATEGY, this );

    // command manager since it is use in the dirty pool object framework
    // the commandable workspace of the target theme is taken
    final IKalypsoFeatureTheme targetTheme = UtilMap.findEditableTheme( mapPanel, IPolyElement.QNAME );
    m_dataModel.setData( ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL, targetTheme.getWorkspace() );

    m_calcUnitTheme = new Model1d2dCalUnitTheme( new I10nString( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitWidget.2" ) ), mapModell ); //$NON-NLS-1$
    m_calcUnitTheme.setLegendIcon( "urn:kalypso:map:theme:swtimage:calculationunittheme:default", null ); //$NON-NLS-1$
    m_calcUnitTheme.setCalculationUnit( m_dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER ) );
    mapModell.insertTheme( m_calcUnitTheme, 0 );
    m_dataModel.addKeyBasedDataChangeListener( calThemeUpdater );

    final IKalypsoFeatureTheme bcTheme = UtilMap.findEditableTheme( mapPanel, IBoundaryCondition.QNAME );
    if( bcTheme == null )
    {
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitWidget.3" ) ); //$NON-NLS-1$
    }
    final CommandableWorkspace bcWorkspace = bcTheme.getWorkspace();
    m_dataModel.setData( ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE, bcWorkspace );
    m_dataModel.setData( ICommonKeys.KEY_GRAB_DISTANCE_PROVIDER, this );

    // set bc list to calunit theme for display
    final Feature bcHolderModelFeature = bcWorkspace.getRootFeature();
    final IFlowRelationshipModel bcModel = (IFlowRelationshipModel) bcHolderModelFeature.getAdapter( IFlowRelationshipModel.class );
    m_calcUnitTheme.setModelBoundaryConditions( bcModel );
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    try
    {
      m_dataModel.setData( ICommonKeys.KEY_SELECTED_DISPLAY, parent.getDisplay() );
      return m_widgetFace.createControl( parent, toolkit );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      return null;
    }
  }

  @Override
  public void disposeControl( )
  {
    m_dataModel.removeAllListeners();
  }

  @Override
  public synchronized boolean canBeActivated( final ISelection selection, final IMapPanel mapPanel )
  {
    return true;
  }

  @Override
  public void finish( )
  {
    try
    {
      final IMapPanel mapPanel = (IMapPanel) m_dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
      final IMapModell mapModell = mapPanel.getMapModell();
      if( mapModell != null && m_calcUnitTheme != null )
      {
        mapModell.removeTheme( m_calcUnitTheme );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    super.finish();
  }

  @Override
  public String getName( )
  {
    return m_name;
  }

  @Override
  public String getToolTip( )
  {
    return m_toolTip;
  }

  @Override
  public String getPartName( )
  {
    return null;
  }
}