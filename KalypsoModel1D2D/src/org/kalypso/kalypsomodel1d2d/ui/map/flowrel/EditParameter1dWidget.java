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
package org.kalypso.kalypsomodel1d2d.ui.map.flowrel;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.jface.dialog.DialogSettingsUtils;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.ITeschkeFlowRelation;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget;
import org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * @author Gernot Belger
 * @author Thomas Jung
 */
public class EditParameter1dWidget extends AbstractDelegateWidget
{
  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  public EditParameter1dWidget( )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter1dWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter1dWidget.1" ), new SelectFeatureWidget( "", "", new QName[] { ITeschkeFlowRelation.QNAME, IFlowRelation1D.QNAME, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    }, IFlowRelationship.QNAME_PROP_POSITION ) );

    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter1dWidget.4" ) ); //$NON-NLS-1$
    m_toolTipRenderer.setBackgroundColor( new Color( 1f, 1f, 0.6f, 0.70f ) );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
    {
      final Rectangle bounds = mapPanel.getScreenBounds();
      final String delegateTooltip = getDelegate().getToolTip();

      m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter1dWidget.5" ) + delegateTooltip ); //$NON-NLS-1$

      m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );

    /* Open the feature view */
    final Display display = PlatformUI.getWorkbench().getDisplay();
    display.asyncExec( new Runnable()
    {
      @Override
      public void run( )
      {
        try
        {
          PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView( "org.kalypso.featureview.views.FeatureView", null, IWorkbenchPage.VIEW_VISIBLE ); //$NON-NLS-1$
        }
        catch( final Throwable pie )
        {
          // final IStatus status = StatusUtilities.statusFromThrowable( pie );
          // KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
          pie.printStackTrace();
        }
      }
    } );
  }

  @Override
  public void keyTyped( final KeyEvent e )
  {
    if( e.getKeyChar() == '\n' )
    {
      e.consume();

      final EasyFeatureWrapper[] features = getMapPanel().getSelectionManager().getAllFeatures();
      final List<IFlowRelationship> flowRels = new ArrayList<>( features.length );
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
        @Override
        public void run( )
        {
          startCalculation( shell, flowRels.toArray( new IFlowRelationship[flowRels.size()] ) );
        }
      } );
      return;
    }
    super.keyTyped( e );
  }

  protected void startCalculation( final Shell shell, final IFlowRelationship[] flowRels )
  {
    final IScenarioDataProvider dataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();

    try
    {
      final IFEDiscretisationModel1d2d discModel = dataProvider.getModel( IFEDiscretisationModel1d2d.class.getName() );
      final IFlowRelationshipModel flowModel = dataProvider.getModel( IFlowRelationshipModel.class.getName() );

      final FlowRelCalcWizard wizard = new FlowRelCalcWizard( flowRels, flowModel, discModel );
      wizard.setDialogSettings( DialogSettingsUtils.getDialogSettings( KalypsoModel1D2DPlugin.getDefault(), "flowRelCalcWizard" ) ); //$NON-NLS-1$
      final WizardDialog2 wizardDialog2 = new WizardDialog2( shell, wizard );
      wizardDialog2.setRememberSize( true );
      if( wizardDialog2.open() == Window.OK )
      {
        // getMapPanel().getSelectionManager().clear();
      }
    }
    catch( final CoreException e )
    {
      KalypsoModel1D2DPlugin.getDefault().getLog().log( e.getStatus() );
      ErrorDialog.openError( shell, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter1dWidget.8" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.EditParameter1dWidget.9" ), e.getStatus() ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#finish()
   */
  @Override
  public void finish( )
  {
    /* Deselect all */
    final IFeatureSelectionManager selectionManager = getMapPanel().getSelectionManager();
    selectionManager.clear();

    super.finish();
  }

}
