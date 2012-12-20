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
package org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolate.INTERPOLATION_METHOD;

/**
 * @author Dejan Antanaskovic
 */
public class Command1D2DTimestepsInterpolationWizard extends Wizard implements INewWizard
{
  private Command1D2DTimestepsInterpolationWizardPage m_page;

  private final Command1D2DTimestepsInterpolate m_commandHandler;

  public Command1D2DTimestepsInterpolationWizard( final Command1D2DTimestepsInterpolate commandHandler, final Double initRelaxationFactor )
  {
    m_commandHandler = commandHandler;
    if( initRelaxationFactor == null || Double.isNaN( initRelaxationFactor ) )
      m_page = new Command1D2DTimestepsInterpolationWizardPage( "1.0" ); //$NON-NLS-1$
    else
      m_page = new Command1D2DTimestepsInterpolationWizardPage( initRelaxationFactor.toString() );
  }

  /**
   * additional constructor
   * 
   * @param initRelaxationFactor
   *          changed to string to allow more flexible expansion of "Relaxation Factor"
   */
  public Command1D2DTimestepsInterpolationWizard( final Command1D2DTimestepsInterpolate commandHandler, final String initRelaxationFactor )
  {
    m_commandHandler = commandHandler;
    if( initRelaxationFactor == null || initRelaxationFactor == "" ) //$NON-NLS-1$
      m_page = new Command1D2DTimestepsInterpolationWizardPage( "1.0" ); //$NON-NLS-1$
    else
      m_page = new Command1D2DTimestepsInterpolationWizardPage( initRelaxationFactor );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  @Override
  public boolean performFinish( )
  {
    try
    {
      final INTERPOLATION_METHOD method = m_page.getInterpolationMethod();
      final int numberOfSteps = m_page.getNumberOfSteps();
      final long timeInterval = m_page.getTimeInterval();
      final String relaxationFactor = m_page.getRelaxationFactorValue();
      m_commandHandler.doInterpolate( method, numberOfSteps, timeInterval, relaxationFactor );
      return true;
    }
    catch( Exception e )
    {
      ErrorDialog.openError( getShell(), getWindowTitle(), e.getLocalizedMessage(), new Status( IStatus.ERROR, KalypsoModel1D2DPlugin.PLUGIN_ID, e.getLocalizedMessage(), e ) );
      return false;
    }
  }

  @Override
  public void addPages( )
  {
    setWindowTitle( Messages.getString( "org.kalypso.kalypsomodel1d2d.ogc.gml.om.table.command.handler.Command1D2DTimestepsInterpolationWizard.1" ) ); //$NON-NLS-1$
    addPage( m_page );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  @Override
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    // TODO Auto-generated method stub
  }

}
