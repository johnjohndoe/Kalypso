/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.internal.gml;

import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IDecoration;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ILightweightLabelDecorator;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.views.tasklist.TaskListMessages;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.MarkerIndex;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.ui.view.table.MarkerUtils;

/**
 * @author Gernot Belger
 */
public class WspmMarkerDecorator implements ILightweightLabelDecorator
{
  @Override
  public void dispose( )
  {
  }

  @Override
  public void addListener( final ILabelProviderListener listener )
  {
  }

  @Override
  public void removeListener( final ILabelProviderListener listener )
  {
  }

  @Override
  public boolean isLabelProperty( final Object element, final String property )
  {
    return true;
  }

  @Override
  public void decorate( final Object element, final IDecoration decoration )
  {
    try
    {
      if( element instanceof IProfileFeature )
        decorateProfile( (IProfileFeature)element, decoration );
      else if( element instanceof TuhhReachProfileSegment )
        decorateProfile( ((TuhhReachProfileSegment)element).getProfileMember(), decoration );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
    }
  }

  private void decorateProfile( final IProfileFeature profileFeature, final IDecoration decoration )
  {
    if( profileFeature == null )
      return;

    final IProfile profile = profileFeature.getProfile();
    if( profile == null )
      return;

    /* find worst marker */
    final MarkerIndex markerIndex = profile.getProblemMarker();
    if( markerIndex == null )
      return;

    final IMarker[] markers = markerIndex.getMarkers();
    final IMarker worst = MarkerUtils.worstOf( markers );
    if( worst == null )
      return;

    final int severity = MarkerUtils.getSeverity( worst );

    /* do not decorate info */
    if( severity == IMarker.SEVERITY_INFO )
      return;

    /* do the decoration */
    final ImageDescriptor imageDescriptor = getImage( severity );

    final String message = worst.getAttribute( IMarker.MESSAGE, null );
    final String kindText = getKindtext( severity );

    decoration.addSuffix( String.format( " (%s: %s)", kindText, message ) ); //$NON-NLS-1$

    decoration.addOverlay( imageDescriptor, IDecoration.BOTTOM_RIGHT );
  }

  private String getKindtext( final int severity )
  {
    switch( severity )
    {
      case IMarker.SEVERITY_ERROR:
        return TaskListMessages.TaskList_error;
      case IMarker.SEVERITY_WARNING:
        return TaskListMessages.TaskList_warning;
      case IMarker.SEVERITY_INFO:
        return TaskListMessages.TaskList_info;
    }

    return null;
  }

  private ImageDescriptor getImage( final int severity )
  {
    final ISharedImages sharedImages = PlatformUI.getWorkbench().getSharedImages();
    switch( severity )
    {
      case IMarker.SEVERITY_ERROR:
        return sharedImages.getImageDescriptor( ISharedImages.IMG_DEC_FIELD_ERROR );
      case IMarker.SEVERITY_WARNING:
        return sharedImages.getImageDescriptor( ISharedImages.IMG_DEC_FIELD_WARNING );
      case IMarker.SEVERITY_INFO:
        return null;
    }

    return null;
  }
}