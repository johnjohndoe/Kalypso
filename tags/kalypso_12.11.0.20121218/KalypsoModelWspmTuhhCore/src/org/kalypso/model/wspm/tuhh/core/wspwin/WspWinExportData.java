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
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.tuhh.core.util.WspmTuhhUtils;
import org.kalypso.wspwin.core.WspCfg.TYPE;

/**
 * @author Gernot Belger
 */
public class WspWinExportData extends AbstractModelObject
{
  public static final String SETTINGS_SECTION_NAME = "WspWinExportData"; //$NON-NLS-1$

  public static final String PROPERTY_OUTPUT_DIR_HISTORY = "outputDirHistory"; //$NON-NLS-1$

  public static final String PROPERTY_OUTPUT_DIR = "outputDir"; //$NON-NLS-1$

  public static final String PROPERTY_OVERWRITE_EXISTING = "overwriteExisting"; //$NON-NLS-1$

  public static final String PROPERTY_PROJECT_TYPE = "projectType"; //$NON-NLS-1$

  public static final String PROPERTY_ROUGHNESS_TYPE = "roughnessType"; //$NON-NLS-1$

  public static final String PROPERTY_PREFER_ROUGHNESS_CLASSES = "preferRoughnessClasses"; //$NON-NLS-1$

  public static final String PROPERTY_PREFER_VEGETATION_CLASSES = "preferVegetationClasses"; //$NON-NLS-1$

  private File m_outputDir;

  private String[] m_outputDirHistory = new String[0];

  private boolean m_overwriteExisting = false;

  private TYPE m_projectType = TYPE.PASCHE;

  /** default: 'both' */
  private String m_roughnessType = StringUtils.EMPTY;

  private boolean m_preferRoughnessClasses = false;

  private boolean m_preferVegetationClasses = false;

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
    final Collection<IProject> wspmProjects = new ArrayList<>();

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

  public void setOverwriteExisting( final boolean overwriteExisting )
  {
    final Object oldValue = m_overwriteExisting;

    m_overwriteExisting = overwriteExisting;

    firePropertyChange( PROPERTY_OVERWRITE_EXISTING, oldValue, overwriteExisting );
  }

  public void loadSettings( final IDialogSettings settings )
  {
    final String[] destinationNames = settings.getArray( PROPERTY_OUTPUT_DIR_HISTORY );
    if( destinationNames != null )
    {
      setOutputDirHistory( destinationNames );

      if( destinationNames.length > 0 )
        setOutputDir( new File( destinationNames[0] ) );
    }

    m_overwriteExisting = settings.getBoolean( PROPERTY_OVERWRITE_EXISTING );

    final String projectType = settings.get( PROPERTY_PROJECT_TYPE );
    if( !StringUtils.isBlank( projectType ) )
      m_projectType = TYPE.valueOf( projectType );
  }

  public void storeSettings( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    final File outputDir = getOutputDir();

    // update source names history
    final String[] history = getOutputDirHistory();
    final Set<String> historySet = new LinkedHashSet<>();
    // New entry on, top; avoid duplicate entries
    if( outputDir != null )
      historySet.add( outputDir.getAbsolutePath() );
    historySet.addAll( Arrays.asList( history ) );

    settings.put( PROPERTY_OUTPUT_DIR_HISTORY, historySet.toArray( new String[historySet.size()] ) );
    settings.put( PROPERTY_OVERWRITE_EXISTING, getOverwriteExisting() );
    settings.put( PROPERTY_PROJECT_TYPE, m_projectType.name() );
  }

  public TYPE getProjectType( )
  {
    return m_projectType;
  }

  public void setProjectType( final TYPE projectType )
  {
    final Object oldValue = m_projectType;

    m_projectType = projectType;

    firePropertyChange( PROPERTY_PROJECT_TYPE, oldValue, projectType );
  }

  public String getRoughnessType( )
  {
    return m_roughnessType;
  }

  public void setRoughnessType( final String roughnessType )
  {
    final String oldValue = m_roughnessType;

    m_roughnessType = roughnessType;

    firePropertyChange( PROPERTY_ROUGHNESS_TYPE, oldValue, roughnessType );
  }

  public boolean getPreferRoughnessClasses( )
  {
    return m_preferRoughnessClasses;
  }

  public void setPreferRoughnessClasses( final boolean preferRoughnessClasses )
  {
    final boolean oldValue = m_preferRoughnessClasses;

    m_preferRoughnessClasses = preferRoughnessClasses;

    firePropertyChange( PROPERTY_PREFER_ROUGHNESS_CLASSES, oldValue, preferRoughnessClasses );
  }

  public boolean getPreferVegetationClasses( )
  {
    return m_preferVegetationClasses;
  }

  public void setPreferVegetationClasses( final boolean preferVegetationClasses )
  {
    final boolean oldValue = m_preferVegetationClasses;

    m_preferVegetationClasses = preferVegetationClasses;

    firePropertyChange( PROPERTY_PREFER_VEGETATION_CLASSES, oldValue, preferVegetationClasses );
  }
}