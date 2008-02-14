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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypso.simulation.core.NullSimulationMonitor;
import org.kalypso.util.swt.StatusComposite;

/**
 * @author Gernot Belger
 */
public class FlowRelCalcSimulationPage extends WizardPage implements IWizardPage
{
  private StatusComposite m_statusComposite;

  private ListViewer m_listViewer;

  private FlowRelationshipCalcOperation m_op;

  public FlowRelCalcSimulationPage( final String pageName )
  {
    super( pageName );
  }

  public FlowRelCalcSimulationPage( final String pageName, final String title, final ImageDescriptor titleImage )
  {
    super( pageName, title, titleImage );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );

    m_statusComposite = new StatusComposite( composite, StatusComposite.DETAILS );
    m_statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    m_listViewer = new ListViewer( composite, SWT.BORDER | SWT.H_SCROLL | SWT.V_SCROLL );
    m_listViewer.getControl().setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    m_listViewer.setContentProvider( new ArrayContentProvider() );
    m_listViewer.setLabelProvider( new LabelProvider() );
    m_listViewer.setUseHashlookup( true );

    setControl( composite );
  }

  public boolean simulationWasRun( )
  {
    return m_op != null;
  }

  public void runSimulation( final TuhhCalculation templateCalculation, final IFlowRelation1D[] flowRels, final IFlowRelationshipModel flowModel, final IFEDiscretisationModel1d2d discModel )
  {
    if( m_op != null )
      return;

    final ListViewer listViewer = m_listViewer;
    final Control control = listViewer.getControl();

    m_statusComposite.setStatus( StatusUtilities.createStatus( IStatus.INFO, "Berechnung gestartet...", null ) );

    final List<String> logList = new ArrayList<String>();
    listViewer.setInput( logList );

    final ISimulationMonitor simulationMonitor = new NullSimulationMonitor()
    {
      /**
       * @see org.kalypso.simulation.core.NullSimulationMonitor#setMessage(java.lang.String)
       */
      @Override
      public void setMessage( final String message )
      {
        logList.add( message );

        final Runnable runnable = new Runnable()
        {
          public void run( )
          {
            listViewer.add( message );
            listViewer.getList().select( logList.size() - 1 );
            listViewer.getList().showSelection();
          }
        };
        control.getDisplay().asyncExec( runnable );
      }
    };

    m_op = new FlowRelationshipCalcOperation( templateCalculation, flowRels, flowModel, discModel, simulationMonitor );
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, m_op );
    m_statusComposite.setStatus( status );
  }

  public IStatus getStatus( )
  {
    return m_statusComposite.getStatus();
  }

  public IStatus applyResults( )
  {
    try
    {
      m_op.applyResults();

      // Post an empty command to flowrelationship model in order to make it dirty
      KalypsoAFGUIFrameworkPlugin.getDefault().getDataProvider().postCommand( IFlowRelationshipModel.class, new EmptyCommand( "", false ) );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );
      KalypsoModel1D2DPlugin.getDefault().getLog().log( status );
      return status;
    }
  }

  public void reset( )
  {
    final IWizardContainer container = getContainer();
    if( container instanceof WizardDialog2 )
      ((WizardDialog2) container).getButton( IDialogConstants.FINISH_ID ).setText( "Start" );

    m_listViewer.setInput( null );
    m_op = null;
    m_statusComposite.setStatus( StatusUtilities.createStatus( IStatus.INFO, "Berechnung noch nicht gestartet", null ) );
  }

}
