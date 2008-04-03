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
package org.kalypso.model.wspm.tuhh.ui.resolutions;

import java.util.StringTokenizer;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.wspm.core.gml.ProfileFeatureFactory;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.reparator.IProfilMarkerResolution;
import org.kalypso.model.wspm.core.profil.validator.IValidatorMarkerCollector;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.editor.gmleditor.ui.GmlEditor;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author kimwerner
 */
public abstract class AbstractProfilMarkerResolution implements IProfilMarkerResolution
{
  private final String m_label;

  private final String m_description;

  private final Image m_image;

  /**
   * 
   */
  public AbstractProfilMarkerResolution( final String label, final String description, final Image image )
  {
    m_label = label;
    m_description = description == null ? label : description;
    m_image = image;
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

  public String getLabel( )
  {
    return m_label;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.reparator.IProfilMarkerResolution#getSerializedParameter()
   */
  public String getSerializedParameter( )
  {
    return getClass().getName();
  }

  private CommandableWorkspace getWorkspace( final IMarker marker )
  {
    try
    {
      final String editorId = (String) marker.getAttribute( IDE.EDITOR_ID_ATTR );
      final IWorkbenchWindow[] wbws = PlatformUI.getWorkbench().getWorkbenchWindows();// .getActiveWorkbenchWindow();
      for( final IWorkbenchWindow wbw : wbws )
      {
        final IEditorReference[] editorReferences = wbw == null ? null : wbw.getActivePage().getEditorReferences();

        if( editorReferences == null )
          return null;
        for( final IEditorReference editorRef : editorReferences )
        {
          if( editorRef.getId().equals( editorId ) )
          {
            final IEditorPart editor = editorRef.getEditor( false );
            final GmlEditor gmlEditor = (editor instanceof GmlEditor) ? (GmlEditor) editor : null;
            return gmlEditor.getTreeView() == null ? null : gmlEditor.getTreeView().getWorkspace();
          }
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
   * @see org.eclipse.ui.IMarkerResolution#run(org.eclipse.core.resources.IMarker)
   */
  public void run( IMarker marker )
  {
    final CommandableWorkspace ws = getWorkspace( marker );

    Feature feature;
    try
    {
      feature = ws == null ? null : ws.getFeature( marker.getAttribute( IValidatorMarkerCollector.MARKER_ATTRIBUTE_PROFILE_ID ).toString() );

      if( feature != null )
      {
        final IProfil profil = ProfileFeatureFactory.toProfile( feature );
        if( profil != null )
        {
          if (resolve( profil ))
            //unnötig wenn der event rausgeschickt wird
            marker.delete();
          ProfileFeatureFactory.toFeature( profil, feature );
          
          // TODO: modell event rausschicken!
        }
      }
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

  }

  protected final String[] getParameter( final String parameterStream )
  {
    StringTokenizer st = new StringTokenizer( parameterStream, ";" );
    final int size = st.countTokens();
    final String[] params = new String[size];
    for( int i = 0; i < size; i++ )
    {
      params[i] = st.nextToken();
    }
    if( !(params[0].equals( getClass().getName() )) )
      throw new IllegalArgumentException();

    return params;
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.reparator.IProfilMarkerResolution#setData(java.lang.String)
   */
  public void setData( String parameterStream )
  {
    getParameter( parameterStream );
  }
}
