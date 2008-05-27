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
package org.kalypso.model.wspm.sobek.core.wizard;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.model.wspm.sobek.core.Messages;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNode;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.model.wspm.sobek.core.interfaces.ILastfall;
import org.kalypso.model.wspm.sobek.core.interfaces.ISobekModelMember;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition.BOUNDARY_CONDITION_TYPE;
import org.kalypso.model.wspm.sobek.core.ui.lastfall.LastfallTreeLabelProvider;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageEditBoundaryConditionGeneral;
import org.kalypso.model.wspm.sobek.core.wizard.pages.PageEditBoundaryConditionTimeSeries;
import org.kalypso.model.wspm.sobek.core.wizard.worker.AbstractTimeSeriesProvider;
import org.kalypso.model.wspm.sobek.core.wizard.worker.FinishWorkerEditBoundaryCondition;
import org.kalypso.model.wspm.sobek.core.wizard.worker.ITimeSeriesProvider;

/**
 * @author kuch
 */
public class SobekWizardEditBoundaryCondition extends Wizard implements INewWizard
{
  private final ILastfall m_lastfall;

  private final IBoundaryNode m_node;

  private IBoundaryNodeLastfallCondition m_condition;

  private PageEditBoundaryConditionTimeSeries m_timeSeries = null;

  private PageEditBoundaryConditionGeneral m_general = null;

  private final LastfallTreeLabelProvider m_provider;

  public SobekWizardEditBoundaryCondition( final ILastfall lastfall, final IBoundaryNode node, final LastfallTreeLabelProvider provider )
  {
    m_lastfall = lastfall;
    m_node = node;
    m_provider = provider;

    setForcePreviousAndNextButtons( true );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  @Override
  public void addPages( )
  {
    if( m_general == null )
    {
      m_general = new PageEditBoundaryConditionGeneral( m_condition );
      addPage( m_general );

      m_timeSeries = new PageEditBoundaryConditionTimeSeries( (ISobekModelMember) m_lastfall.getModelMember(), m_condition, m_general );
      addPage( m_timeSeries );
    }
  }

  public IBoundaryNodeLastfallCondition getBoundaryNodeLastfallCondition( )
  {
    return m_condition;
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    try
    {
      m_condition = m_node.getLastfallCondition( m_lastfall );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  @Override
  public boolean performFinish( )
  {
    final BOUNDARY_CONDITION_TYPE type = m_timeSeries.getTypeOfTimeSeries();
    final ITimeSeriesProvider provider = AbstractTimeSeriesProvider.createProvider( type, m_general, m_timeSeries );

    final ICoreRunnableWithProgress worker = new FinishWorkerEditBoundaryCondition( m_lastfall, m_node, provider );

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, worker );
    ErrorDialog.openError( getShell(), getWindowTitle(), Messages.SobekWizardEditBoundaryCondition_0, status );

    m_provider.updateIcons();

    if( status.isOK() )
      return true;

    return false;
  }

}
