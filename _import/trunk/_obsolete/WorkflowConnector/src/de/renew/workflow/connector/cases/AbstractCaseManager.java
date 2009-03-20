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
package de.renew.workflow.connector.cases;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBContext;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.contribs.eclipse.core.resources.FolderUtilities;

import de.renew.workflow.cases.CaseList;
import de.renew.workflow.connector.WorkflowConnectorPlugin;

/**
 * @author Stefan Kurzbach
 */
public abstract class AbstractCaseManager<T extends ICase> implements ICaseManager<T>
{
  public static final String METADATA_FOLDER = ".metadata";

  public static final String METADATA_FILENAME = "cases.xml";

  private final JAXBContext m_jc;

  private final List<ICaseManagerListener<T>> m_listeners = new ArrayList<ICaseManagerListener<T>>();

  private ICaseList m_cases;

  private T m_currentCase;

  protected final IProject m_project;

  private final IFile m_metaDataFile;

  /**
   * Initializes the {@link ICaseManager} on the given project
   * 
   * @param project
   *          the project, must not be <code>null</code>
   * @exception CoreException
   *              if this method fails. Reasons include:
   *              <ul>
   *              <li>The metadata folder is not accessible.</li>
   *              <li>There is a problem loading the database.</li>
   */
  public AbstractCaseManager( final IProject project, final JAXBContext jc ) throws CoreException
  {
    m_jc = jc;
    m_project = project;

    final IFolder folder = project.getFolder( METADATA_FOLDER );
    if( !folder.exists() )
    {
      try
      {
        folder.create( false, true, null );
      }
      catch( final CoreException e )
      {
        // ignore
        e.printStackTrace();
      }
    }

    final IFile metadataFile = folder.getFile( METADATA_FILENAME );
    m_metaDataFile = metadataFile;
    loadModel();
  }

  private void loadModel( ) throws CoreException
  {
    if( !m_metaDataFile.exists() )
    {
      final CaseList cases = new de.renew.workflow.cases.ObjectFactory().createCaseList();
      m_cases = new CaseListHandler( cases, m_project );
      createCase( "Basis" );
    }
    else
    {
      try
      {
        final URL url = m_metaDataFile.getRawLocationURI().toURL();
        final CaseList cases = (CaseList) m_jc.createUnmarshaller().unmarshal( url );
        m_cases = new CaseListHandler( cases, m_project );
      }
      catch( final Throwable e )
      {
        final IStatus status = new Status( Status.ERROR, WorkflowConnectorPlugin.PLUGIN_ID, "", e );
        throw new CoreException( status );
      }
    }
  }

  public abstract T createCase( final String name ) throws CoreException;

  public abstract void removeCase( final T caze, IProgressMonitor monitor ) throws CoreException;

  protected final void internalAddCase( final ICase caze )
  {
    m_cases.getCaseList().getCases().add( caze.getCase() );
  }

  protected final void internalRemoveCase( final ICase caze )
  {
    m_cases.getCaseList().getCases().remove( caze.getCase() );
  }

  /**
   * @see de.renew.workflow.connector.context.ICaseManager#getCurrentCase()
   */
  public T getCurrentCase( )
  {
    return m_currentCase;
  }

  /**
   * @see de.renew.workflow.connector.context.ICaseManager#setCurrentCase(de.renew.workflow.cases.Case)
   */
  public void setCurrentCase( final T caze )
  {
    m_currentCase = caze;
  }

  /**
   * @see de.renew.workflow.connector.context.ICaseManager#getCases()
   */
  public List<ICase> internalGetCases( )
  {
    return m_cases.getCases();
  }

  public void persist( @SuppressWarnings("unused") final IProgressMonitor monitor )
  {
    // TODO: comment, why in a job?!
    final Job job = new Job( "" )
    {
      /**
       * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
       */
      @SuppressWarnings("synthetic-access")
      @Override
      protected IStatus run( @SuppressWarnings("hiding") final IProgressMonitor monitor )
      {
        try
        {
          monitor.beginTask( "Szenarien speichern.", 5000 );
          final ByteArrayOutputStream bos = new ByteArrayOutputStream();
          m_jc.createMarshaller().marshal( m_cases.getCaseList(), bos );
          bos.close();
          monitor.worked( 2000 );
          final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );
          m_metaDataFile.refreshLocal( 0, null );
          if( m_metaDataFile.exists() )
            m_metaDataFile.setContents( bis, false, true, null );
          else
          {
            FolderUtilities.mkdirs( m_metaDataFile.getParent() );
            m_metaDataFile.create( bis, false, null );
          }

          bis.close();
        }
        catch( final Exception e )
        {
          return new Status( Status.ERROR, WorkflowConnectorPlugin.PLUGIN_ID, "", e );
        }
        finally
        {
          monitor.done();
        }
        return Status.OK_STATUS;
      }
    };
    job.setRule( m_metaDataFile.getParent().getParent() );
    job.schedule();
  }

  /**
   * @see de.renew.workflow.connector.context.ICaseManager#addCaseManagerListener(de.renew.workflow.connector.context.ICaseManagerListener)
   */
  public void addCaseManagerListener( final ICaseManagerListener<T> l )
  {
    if( l == null )
    {
      return;
    }
    else
    {
      if( m_listeners.contains( l ) )
      {
        return;
      }
      else
      {
        m_listeners.add( l );
      }
    }
  }

  /**
   * @see de.renew.workflow.connector.context.ICaseManager#removeCaseManagerListener(de.renew.workflow.connector.context.ICaseManagerListener)
   */
  public void removeCaseManagerListener( final ICaseManagerListener<T> l )
  {
    if( l == null )
    {
      return;
    }
    else
    {
      m_listeners.remove( l );
    }
  }

  protected void fireCaseAdded( final T caze )
  {
    for( final ICaseManagerListener<T> l : m_listeners )
    {
      l.caseAdded( caze );
    }
  }

  protected void fireCaseRemoved( final T caze )
  {
    for( final ICaseManagerListener<T> l : m_listeners )
    {
      l.caseRemoved( caze );
    }
  }

  /**
   * @see de.renew.workflow.connector.context.ICaseManager#dispose()
   */
  public void dispose( )
  {
    m_listeners.clear();
  }
}
