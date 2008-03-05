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
package org.kalypso.model.wspm.tuhh.ui.resolutions;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IMarkerResolution2;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.ui.GmlEditor;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kimwerner
 */
public abstract class AbstractProfilMarkerResolution implements IMarkerResolution2
{
  /**
   * @see org.eclipse.ui.IMarkerResolution#run(org.eclipse.core.resources.IMarker)
   */
  public void run( IMarker marker )
  {
    final Feature feature = getFeature( marker );
    if( feature != null )
    {
      final IProfil profil = ProfileFeatureFactory.toProfile( feature );
      if( profil != null )
      {
        final boolean resolved = resolve( profil );
        ProfileFeatureFactory.toFeature( profil, feature );
        if( resolved )
          try
          {
            marker.delete();
          }
          catch( CoreException e )
          {
            // TODO Auto-generated catch block
            e.printStackTrace();
          }
      }
    }
  }

  private final String m_label;

  private final String m_description;

  private final Image m_image;

  private Feature getFeature( final IMarker marker )
  {
    try
    {
      final String editorId = (String) marker.getAttribute( IDE.EDITOR_ID_ATTR );
      final IEditorReference[] editorReferences = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().getEditorReferences();
      if( editorReferences == null )
        return null;
      for( final IEditorReference editorRef : editorReferences )
      {
        if( editorRef.getId().equals( editorId ) )
        {
          final IEditorPart editor = editorRef.getEditor( false );
          final GmlEditor gmlEditor = (editor instanceof GmlEditor) ? (GmlEditor) editor : null;
          final CommandableWorkspace ws = (gmlEditor.getTreeView() == null) ? null : gmlEditor.getTreeView().getWorkspace();
          return (ws == null) ? null : ws.getFeature( marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_PROFILE_ID ).toString() );
        }
      }
    }
    catch( final CoreException e )
    {
      KalypsoModelWspmTuhhUIPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      return null;
    }
    return null;
  }

  /**
   * 
   */
  public AbstractProfilMarkerResolution( final String label, final String description, final Image image )
  {
    m_label = label;
    m_description = description;
    m_image = image;
  }

  public String getLabel( )
  {
    return m_label;
  }

  /**
   * @see org.eclipse.ui.IMarkerResolution2#getDescription()
   */
  public String getDescription( )
  {

    return m_description;
  }

  /**
   * @see org.eclipse.ui.IMarkerResolution2#getImage()
   */
  public Image getImage( )
  {
    return m_image;
  }

  protected abstract boolean resolve( final IProfil profil );
}
