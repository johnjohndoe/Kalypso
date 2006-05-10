/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.featureview.dialog;

import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.ui.dialogs.KalypsoResourceSelectionDialog;
import org.kalypso.contribs.eclipse.ui.dialogs.ResourceSelectionValidator;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.ogc.gml.featureview.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * @author Nadja Peiler (09.06.2005)
 */
public class ResourceFileDialog implements IFeatureDialog
{
  private Feature m_feature;

  private IPropertyType m_ftp;

  private FeatureChange m_change;

  public ResourceFileDialog( final Feature feature, final IPropertyType ftp )
  {
    m_feature = feature;
    m_ftp = ftp;
  }

  public int open( Shell shell )
  {
    final GMLWorkspace workspace = m_feature.getWorkspace();
    final IFile gmlFile = ResourceUtilities.findFileFromURL( workspace.getContext() );
    IWorkspaceRoot workspaceRoot = gmlFile.getWorkspace().getRoot();
    IResource resultFile = null;
    resultFile = getResourceFile();
    if( resultFile == null )
      resultFile = workspaceRoot;
    KalypsoResourceSelectionDialog selectionDialog = new KalypsoResourceSelectionDialog( shell, resultFile, "", null, workspaceRoot, new ResourceSelectionValidator() );
    final int open;
    selectionDialog.open();
    Object[] result = selectionDialog.getResult();
    if( result != null )
    {
      Path resultPath = (Path) result[0];

      m_change = new FeatureChange( m_feature, m_ftp, ResourceUtilities.findFileFromPath( resultPath ) );
      open = Window.OK;
    }
    else
    {
      open = Window.CANCEL;
    }
    return open;
  }

  private IFile getResourceFile( )
  {
    if( m_change != null )
      return (IFile) m_change.getNewValue();

    return (IFile) m_feature.getProperty( m_ftp );
  }

  /**
   * @see org.kalypso.ogc.gml.featureview.dialog.IFeatureDialog#collectChanges(java.util.Collection)
   */
  public void collectChanges( final Collection<FeatureChange> c )
  {
    if( c != null && m_change != null )
      c.add( m_change );
  }

  public String getLabel( )
  {
    String label = "Choose...";
    if( getResourceFile() != null )
      label = getResourceFile().toString();
    return label;
  }

}