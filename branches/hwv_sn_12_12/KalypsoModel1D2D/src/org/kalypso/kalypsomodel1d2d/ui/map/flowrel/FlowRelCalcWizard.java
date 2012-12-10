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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IPageChangingListener;
import org.eclipse.jface.dialogs.PageChangingEvent;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.wizard.WizardDialog2;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IFlowRelation1D;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationship;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhCalculation;
import org.kalypso.util.swt.ListSelectionWizardPage;

/**
 * @author Gernot Belger
 */
public class FlowRelCalcWizard extends Wizard implements INewWizard
{
  private final IFlowRelationship[] m_flowRels;

  private ListSelectionWizardPage m_chooseFlowsRelPage;

  private FlowRelCalcControlPage m_controlPage;

  private FlowRelCalcSimulationPage m_simulationPage;

  private final IFlowRelationshipModel m_flowModel;

  private final IFEDiscretisationModel1d2d m_discModel;

  private final IPageChangingListener m_pageListener = new IPageChangingListener()
  {
    @Override
    public void handlePageChanging( final PageChangingEvent event )
    {
      FlowRelCalcWizard.this.handlePageChanging( event );
    }
  };

  @Override
  public void createPageControls( Composite pageContainer )
  {
    super.createPageControls( pageContainer );

    m_chooseFlowsRelPage.setInput( m_flowRels );
    // m_chooseFlowsRelPage.setCheckedElements( m_flowRels );
    m_chooseFlowsRelPage.setTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcWizard.1" ) ); //$NON-NLS-1$
    m_chooseFlowsRelPage.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcWizard.2" ) ); //$NON-NLS-1$
    m_chooseFlowsRelPage.setAllowNextIfEmpty( false );

    m_controlPage.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcWizard.5" ) ); //$NON-NLS-1$

    m_simulationPage.setMessage( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcWizard.8" ) ); //$NON-NLS-1$

    setWindowTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcWizard.9" ) ); //$NON-NLS-1$

  }

  @Override
  public void addPages( )
  {
    super.addPages();
    m_chooseFlowsRelPage = new ListSelectionWizardPage( "selectFLowRelsPage", new LabelProvider() //$NON-NLS-1$
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        final IFlowRelationship flowRel = (IFlowRelationship) element;
        return flowRel.getName();
      }
    } );

    m_controlPage = new FlowRelCalcControlPage( "controlPage", Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcWizard.4" ), null ); //$NON-NLS-1$ //$NON-NLS-2$

    m_simulationPage = new FlowRelCalcSimulationPage( "simulationPage", Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.flowrel.FlowRelCalcWizard.7" ), null ); //$NON-NLS-1$ //$NON-NLS-2$

    addPage( m_chooseFlowsRelPage );
    addPage( m_controlPage );
    addPage( m_simulationPage );

    setNeedsProgressMonitor( true );
  }

  public FlowRelCalcWizard( final IFlowRelationship[] flowRels, final IFlowRelationshipModel flowModel, final IFEDiscretisationModel1d2d discModel )
  {
    m_flowRels = flowRels;
    m_flowModel = flowModel;
    m_discModel = discModel;
  }

  protected void handlePageChanging( final PageChangingEvent event )
  {
    if( event.getCurrentPage() == m_chooseFlowsRelPage && event.getTargetPage() == m_controlPage )
    {
      final Object[] selection = m_chooseFlowsRelPage.getSelection();
      final IFlowRelation1D[] flowRels1 = new IFlowRelation1D[selection.length];
      for( int i = 0; i < selection.length; i++ )
        flowRels1[i] = (IFlowRelation1D) selection[i];
      final IFlowRelation1D[] flowRels = flowRels1;
      m_controlPage.setCalculation( flowRels );
    }

    if( event.getTargetPage() == m_simulationPage )
    {
      final IFlowRelation1D[] flowRels = m_controlPage.getFlowRels();
      final TuhhCalculation templateCalculation = m_controlPage.getTemplate();
      m_simulationPage.reset( templateCalculation, flowRels, m_flowModel, m_discModel );
    }
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#canFinish()
   */
  @Override
  public boolean canFinish( )
  {
    if( getContainer().getCurrentPage() == m_simulationPage )
    {
      final IStatus status = m_simulationPage.getStatus();
      if( status != null && status.matches( IStatus.CANCEL ) )
        return false;

      return true;
    }

    return false;
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#setContainer(org.eclipse.jface.wizard.IWizardContainer)
   */
  @Override
  public void setContainer( final IWizardContainer wizardContainer )
  {
    final IWizardContainer currentContainer = getContainer();
    if( currentContainer instanceof WizardDialog )
      ((WizardDialog) currentContainer).removePageChangingListener( m_pageListener );

    super.setContainer( wizardContainer );

    if( wizardContainer instanceof WizardDialog )
      ((WizardDialog) wizardContainer).addPageChangingListener( m_pageListener );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    final IWizardContainer container = getContainer();

    if( m_simulationPage.simulationWasRun() )
    {
      // Only really change the data on OK
      m_simulationPage.applyResults();
      return true;
    }

    /* Simulation was not yet run, do it now. */
    container.showPage( m_simulationPage );
    m_simulationPage.runSimulation();

    if( container instanceof WizardDialog2 )
    {
      final Button button = ((WizardDialog2) container).getButton( IDialogConstants.FINISH_ID );
      if( button != null )
        button.setText( IDialogConstants.OK_LABEL );
    }
    return false;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  @Override
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
  }

}
