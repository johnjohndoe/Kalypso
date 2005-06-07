/*
 * --------------- Kalypso-Header
 * --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.floodrisk.wizard;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.jobs.ILock;
import org.eclipse.jface.wizard.Wizard;
import org.kalypso.floodrisk.process.ProcessExtension;
import org.kalypso.floodrisk.process.impl.ProcessJob;
import org.kalypso.model.xml.ModeldataType;
import org.kalypso.model.xml.ObjectFactory;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * ProcessInputWizard
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (18.05.2005)
 */
public class ProcessInputWizard extends Wizard
{

  private IProject m_project;

  private ProcessExtension[] m_processes;

  public ProcessInputWizard( IProject project, ProcessExtension[] processes )
  {
    super();
    m_project = project;
    m_processes = processes;
    setWindowTitle( "Process Input Wizard" );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  public void addPages()
  {
    super.addPages();

    for( int i = 0; i < m_processes.length; i++ )
    {
      if( m_processes[i].getState() )
      {
        ProcessInputWizardPage page = new ProcessInputWizardPage(
            m_processes[i].getName(), m_processes[i].getName(), null );
        page.setProcessExtension( m_processes[i] );
        page.setProject( m_project );
        addPage( page );
      }
    }
  }

  public boolean performFinish()
  {

    try
    {
      final ILock lock = Platform.getJobManager().newLock();
      for( int i = 0; i < m_processes.length; i++ )
      {
        if( m_processes[i].getState() )
        {
          //read ModelData
          ModeldataType modelData = readModelData( m_processes[i]
              .getModelDataPath() );
          //check if typeID in modelData equals typeID of process
          if( !m_processes[i].getId().equals( modelData.getTypeID() ) )
          {
            throw new CoreException( KalypsoGisPlugin.createErrorStatus(
                "TypeId of modelData (" + modelData.getTypeID()
                    + ") does not fit to typeID of process ("
                    + m_processes[i].getId() + ")! Check modelData!", null ) );
          }
          //create job
          ProcessJob processJob = new ProcessJob( modelData, m_project,
              m_processes[i], lock );
          //run job
          processJob.schedule();
        }
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    return true;
  }

  private ModeldataType readModelData( IPath modelDataPath )
      throws CoreException
  {
    try
    {
      final IFile file = m_project.getFile( modelDataPath
          .removeFirstSegments( 1 ) );

      final ObjectFactory factory = new ObjectFactory();
      final Unmarshaller unmarshaller = factory.createUnmarshaller();
      return (ModeldataType)unmarshaller.unmarshal( file.getContents() );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();

      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Laden der Modell-Spezifikation", e ) );
    }
  }

}