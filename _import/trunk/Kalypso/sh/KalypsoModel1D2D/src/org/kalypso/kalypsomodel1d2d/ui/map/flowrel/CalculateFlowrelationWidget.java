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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.scenarios.SzenarioDataProvider;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBuildingFlowRelation;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.ui.map.AbstractSelectFeatureWidget;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class CalculateFlowrelationWidget extends AbstractSelectFeatureWidget
{
  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private Point m_point;

  public CalculateFlowrelationWidget( )
  {
    super( "parameter1Dneuberechnen", "xxxx", true, new QName[] { ITeschkeFlowRelation.QNAME, IBuildingFlowRelation.QNAME }, IFlowRelationship.QNAME_PROP_POSITION );

    m_toolTipRenderer.setTooltip( "Selektieren Sie Parameter in der Karte.\nDrücken Sie 'Enter' um die Berechnung zu starten." );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#moved(java.awt.Point)
   */
  @Override
  public void moved( final Point p )
  {
    super.moved( p );

    m_point = p;

    getMapPanel().repaint();
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    if( m_point != null )
      m_toolTipRenderer.paintToolTip( m_point, g, g.getClipBounds() );
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.ui.map.AbstractSelectFeatureWidget#featureGrabbed(org.kalypso.ogc.gml.mapmodel.CommandableWorkspace,
   *      org.kalypsodeegree.model.feature.Feature[])
   */
  @Override
  protected void featureGrabbed( final CommandableWorkspace workspace, final Feature[] selectedFeatures ) throws Exception
  {
    // Toggle selection
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();

    if( selectedFeatures.length == 0 )
      selectionManager.clear();

    final List<Feature> toRemove = new ArrayList<Feature>();
    final List<EasyFeatureWrapper> toAdd = new ArrayList<EasyFeatureWrapper>();

    for( final Feature feature : selectedFeatures )
    {
      if( selectionManager.isSelected( feature ) )
        toRemove.add( feature );
      else
        toAdd.add( new EasyFeatureWrapper( workspace, feature, feature.getParent(), feature.getParentRelation() ) );
    }

    selectionManager.changeSelection( toRemove.toArray( new Feature[toRemove.size()] ), toAdd.toArray( new EasyFeatureWrapper[toAdd.size()] ) );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyTyped(java.awt.event.KeyEvent)
   */
  @Override
  public void keyTyped( final KeyEvent e )
  {
    if( e.getKeyChar() == '\n' )
    {
      e.consume();

      final EasyFeatureWrapper[] features = getMapPanel().getSelectionManager().getAllFeatures();
      final List<IFlowRelationship> flowRels = new ArrayList<IFlowRelationship>( features.length );
      for( final EasyFeatureWrapper feature : features )
      {
        final IFlowRelationship adapter = (IFlowRelationship) feature.getFeature().getAdapter( IFlowRelationship.class );
        if( adapter != null )
          flowRels.add( adapter );
      }

      // Force it into swt
      final IHandlerService service = (IHandlerService) PlatformUI.getWorkbench().getService( IHandlerService.class );
      final Shell shell = (Shell) service.getCurrentState().getVariable( ISources.ACTIVE_SHELL_NAME );
      shell.getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          startCalculation( shell, flowRels.toArray( new IFlowRelationship[flowRels.size()] ) );
        }
      } );
    }
  }

  protected void startCalculation( final Shell shell, final IFlowRelationship[] flowRels )
  {
    final SzenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider();

    try
    {
      final IFEDiscretisationModel1d2d discModel = dataProvider.getModel( IFEDiscretisationModel1d2d.class );
      final IFlowRelationshipModel flowModel = dataProvider.getModel( IFlowRelationshipModel.class );

      final IWizard wizard = new FlowRelCalcWizard( flowRels, flowModel, discModel );
      final WizardDialog2 wizardDialog2 = new WizardDialog2( shell, wizard );
      if( wizardDialog2.open() == Window.OK )
        getMapPanel().getSelectionManager().clear();
    }
    catch( final CoreException e )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( e.getStatus() );
      ErrorDialog.openError( shell, "Parameter berechnen", "Modelldaten stehen nicht zur Verfügung", e.getStatus() );
    }
  }
}
