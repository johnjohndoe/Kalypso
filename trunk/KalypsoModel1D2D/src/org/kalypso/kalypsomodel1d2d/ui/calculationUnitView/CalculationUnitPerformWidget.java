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
package org.kalypso.kalypsomodel1d2d.ui.calculationUnitView;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.Util;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.i18n.I10nString;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.CalculationUnitDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ui.map.merge.Model1d2dCalUnitTheme;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.widgets.AbstractWidget;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Madanagopal
 */
public class CalculationUnitPerformWidget extends AbstractWidget implements IWidgetWithOptions
{
  private final CalculationUnitDataModel m_dataModel = new CalculationUnitDataModel();

  public CalculationUnitPerformWidget( )
  {
    this( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitPerformWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitPerformWidget.1" ) ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public CalculationUnitPerformWidget( final String name, final String toolTip )
  {
    super( name, toolTip );
  }

  @Override
  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    try
    {
      m_dataModel.setData( ICommonKeys.KEY_SELECTED_DISPLAY, parent.getDisplay() );

      final CalculationUnitPerformWidgetFace calcWidgetFace = new CalculationUnitPerformWidgetFace( m_dataModel );
      return calcWidgetFace.createControl( parent, toolkit );
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
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    m_dataModel.setData( ICommonKeys.KEY_MAP_PANEL, mapPanel );
    final IMapModell mapModell = mapPanel.getMapModell();

    final IScenarioDataProvider modelProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    try
    {
      m_dataModel.setData( ICommonKeys.KEY_DATA_PROVIDER, modelProvider );
      final IFEDiscretisationModel1d2d model = modelProvider.getModel( IFEDiscretisationModel1d2d.class.getName() );
      m_dataModel.setData( ICommonKeys.KEY_DISCRETISATION_MODEL, model );
      m_dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, CalcUnitOps.getModelCalculationUnits( model ) );
      m_dataModel.setData( ICommonKeys.WIDGET_WITH_STRATEGY, this );

      final Model1d2dCalUnitTheme calcUnitTheme = new Model1d2dCalUnitTheme( new I10nString( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.calculationUnitView.CalculationUnitPerformWidget.2" ) ), mapModell ); //$NON-NLS-1$
      calcUnitTheme.setLegendIcon( "urn:kalypso:map:theme:swtimage:calculationunittheme:default", null ); //$NON-NLS-1$
      calcUnitTheme.setCalculationUnit( m_dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER ) );
      mapModell.insertTheme( calcUnitTheme, 0 );

      final CalculationUnitDataModel dataModel = m_dataModel;
      dataModel.addKeyBasedDataChangeListener( new KeyBasedDataModelChangeListener()
      {
        @Override
        public void dataChanged( final String key, final Object newValue )
        {
          if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) )
          {
            calcUnitTheme.setCalculationUnit( (ICalculationUnit) newValue );
          }
        }
      } );

      // command manager since it is used in the dirty pool object framework
      // the commandable workspace of the target theme is taken
      // TODO: that cannot work, as the models workspace is not a commandable workspace
      m_dataModel.setData( ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL, model.getWorkspace() );

      m_dataModel.setData( ICommonKeys.KEY_GRAB_DISTANCE_PROVIDER, this );
      m_dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, CalcUnitOps.getModelCalculationUnits( model ) );

      final IKalypsoFeatureTheme operationalTheme = UtilMap.findEditableTheme( mapPanel, IBoundaryCondition.QNAME );
      m_dataModel.setData( ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE, operationalTheme.getWorkspace() );

      final IFlowRelationshipModel bcModel = Util.getModel( IFlowRelationshipModel.class.getName() );

      calcUnitTheme.setModelBoundaryConditions( bcModel );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

  }

  @Override
  public void finish( )
  {
    try
    {
      final IMapPanel mapPanel = (IMapPanel) m_dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
      final IMapModell mapModell = mapPanel.getMapModell();
      if( mapModell != null )
      {
        final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
        for( final IKalypsoTheme kalypsoTheme : allThemes )
        {
          if( kalypsoTheme instanceof Model1d2dCalUnitTheme )
            mapModell.removeTheme( kalypsoTheme );
        }
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    super.finish();
  }

  @Override
  public String getPartName( )
  {
    return null;
  }
}