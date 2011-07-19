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
package org.kalypso.model.wspm.tuhh.core.wspwin;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.databinding.observable.set.WritableSet;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.tuhh.core.util.WspmTuhhUtils;

/**
 * @author Gernot Belger
 */
public class WspWinExportData extends AbstractModelObject
{
  public static final String PROPERTY_OUTPUT_DIR_HISTORY = "outputDirHistory"; //$NON-NLS-1$

  public static final String PROPERTY_OUTPUT_DIR = "outputDir"; //$NON-NLS-1$

  public static final String PROPERTY_OVERWRITE_EXISTING = "overwriteExisting"; //$NON-NLS-1$

  private final WritableSet m_selectedProjects = new WritableSet();

  private File m_outputDir;

  private String[] m_outputDirHistory = new String[0];

  private boolean m_overwriteExisting = false;

  public void setSelection( final IStructuredSelection selection )
  {
    for( final Object element : selection.toArray() )
    {
      if( element instanceof IResource )
        m_selectedProjects.add( ((IResource) element).getProject() );
    }
  }

  public WritableSet getSelectedProjectList( )
  {
    return m_selectedProjects;
  }

  public IProject[] getSelectedProjects( )
  {
    return (IProject[]) m_selectedProjects.toArray( new IProject[m_selectedProjects.size()] );
  }

  public File getOutputDir( )
  {
    return m_outputDir;
  }

  public String[] getOutputDirHistory( )
  {
    return m_outputDirHistory;
  }

  public void setOutputDirHistory( final String[] history )
  {
    final String[] oldValue = history;

    m_outputDirHistory = history;

    firePropertyChange( PROPERTY_OUTPUT_DIR_HISTORY, oldValue, history );
  }

  public void setOutputDir( final File outputDir )
  {
    final Object oldValue = m_outputDir;

    m_outputDir = outputDir;

    firePropertyChange( PROPERTY_OUTPUT_DIR, oldValue, outputDir );
  }

  public IProject[] getWspmProjects( )
  {
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IProject[] projects = workspace.getRoot().getProjects();
    final Collection<IProject> wspmProjects = new ArrayList<IProject>();

    for( final IProject project : projects )
    {
      if( WspmTuhhUtils.isWspmTuhhProject( project ) )
        wspmProjects.add( project );
    }    

    return wspmProjects.toArray( new IProject[wspmProjects.size()] );
  }

  public boolean getOverwriteExisting( )
  {
    return m_overwriteExisting;
  }

  public void setOverwriteExsiting( final boolean overwriteExisting )
  {
    final Object oldValue = m_overwriteExisting;

    m_overwriteExisting = overwriteExisting;

    firePropertyChange( PROPERTY_OVERWRITE_EXISTING, oldValue, overwriteExisting );
  }

  public void loadSettings( final IDialogSettings settings )
  {
    final String[] destinationNames = settings.getArray( PROPERTY_OUTPUT_DIR_HISTORY );
    if( destinationNames != null )
      setOutputDirHistory( destinationNames );
  }

  public void storeSettings( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    // update source names history
    String[] history = getOutputDirHistory();
    final File outputDir = getOutputDir();
    if( outputDir != null )
      history = (String[]) ArrayUtils.add( history, outputDir.getAbsolutePath() );

    settings.put( PROPERTY_OUTPUT_DIR_HISTORY, history );
    settings.put( PROPERTY_OVERWRITE_EXISTING, getOverwriteExisting() );
  }
}