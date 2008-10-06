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
package org.kalypso.model.wspm.ui.view.table.swt;

import java.util.Collection;
import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.kalypso.contribs.eclipse.jface.viewers.ITooltipProvider;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.core.profil.IProfilPointProperty;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;

/**
 * @author Belger
 */
public class ProfilLabelProvider extends LabelProvider implements ITableLabelProvider, ITableColorProvider, ITooltipProvider
{
  private static final String IMAGE_ERROR = "profilLabelProvider.img.error";

  private static final String IMAGE_WARNING = "profilLabelProvider.img.warning";

  private final ImageRegistry m_imgRegistry = new ImageRegistry();

  public static final String COLUMN_KEY = "columnKey";

  public final static String ENTRY_FORMAT = "%.4f";

  private final TableViewer m_viewer;

  private Color m_colorError;

  private Color m_colorWarning;

  public ProfilLabelProvider( final TableViewer viewer )
  {
    m_viewer = viewer;

    m_imgRegistry.put( IMAGE_ERROR, KalypsoModelWspmUIImages.ID_MARKER_ERROR );
    m_imgRegistry.put( IMAGE_WARNING, KalypsoModelWspmUIImages.ID_MARKER_WARNING );

    final IProfilPointMarkerProvider[] mps = KalypsoModelWspmCoreExtensions.getAllMarkerProviders();
    for( final IProfilPointMarkerProvider mp : mps )
    {
      for( final String markerId : mp.getMarkerTypes() )
      {
        final ImageDescriptor img = mp.getImageFor( markerId );
        if( img != null )
          m_imgRegistry.put( markerId, img );
      }
    }
    final Display display = m_viewer.getControl().getDisplay();
    m_colorError = display.getSystemColor( SWT.COLOR_RED );
    m_colorWarning = display.getSystemColor( SWT.COLOR_YELLOW );

  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
   */
  public Image getColumnImage( final Object element, final int columnIndex )
  {
    final IProfilPointProperty column = (IProfilPointProperty) m_viewer.getTable().getColumn( columnIndex ).getData( COLUMN_KEY );

    if( column == null )
    {
      final IContentProvider contentProvider = m_viewer.getContentProvider();
      if( contentProvider instanceof ProfilContentProvider )
      {
        final Map<IProfilPoint, Collection<IMarker>> markerIndex = ((ProfilContentProvider) contentProvider).getMarkerIndex();
        final Collection<IMarker> markers = markerIndex.get( element );
        final Integer severity = worstOf( markers );
        final String deviderTyp = (element instanceof IProfilPoint) ? getDeviderTyp( (IProfilPoint) element ) : null;
        if( severity != null )
        {
          if( IMarker.SEVERITY_ERROR == severity )
            return m_imgRegistry.get( IMAGE_ERROR );
          if( IMarker.SEVERITY_WARNING == severity )
            return m_imgRegistry.get( IMAGE_WARNING );
        }
        return m_imgRegistry.get( deviderTyp );
      }
    }
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
   */
  public String getColumnText( final Object element, final int columnIndex )
  {
    final IProfilPointProperty column = (IProfilPointProperty) m_viewer.getTable().getColumn( columnIndex ).getData( COLUMN_KEY );
    if( column == null )
      return null;

    if( element instanceof IProfilPoint )
    {
      final IProfilPoint row = (IProfilPoint) element;

      try
      {
        if( row.hasProperty( column.getId() ) )
          return String.format( ENTRY_FORMAT, row.getValueFor( column.getId() ) );
        return "";
      }

      catch( final Exception e )
      {
        // should never happen

        e.printStackTrace();

        return null;
      }
    }

    return element.toString() + " - " + columnIndex;
  }

  private Integer worstOf( final Collection<IMarker> markers )
  {
    if( markers == null )
      return null;

    Integer severity = null;
    try
    {
      for( final IMarker marker : markers )
      {
        // if it is an error, immediatly return
        final Integer sev = (Integer) marker.getAttribute( IMarker.SEVERITY );
        if( sev.equals( IMarker.SEVERITY_ERROR ) )
          return sev;

        // else remember it, but maybe it gets worse
        if( sev.equals( IMarker.SEVERITY_WARNING ) )
          severity = sev;
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

    return severity;
  }

  public String getTooltip( final Object element )
  {
    final IContentProvider contentProvider = m_viewer.getContentProvider();
    if( contentProvider instanceof ProfilContentProvider )
    {
      final Map<IProfilPoint, Collection<IMarker>> markerIndex = ((ProfilContentProvider) contentProvider).getMarkerIndex();
      final Collection<IMarker> markers = markerIndex.get( element );
      final StringBuffer sb = new StringBuffer();
      if( markers != null )
      {
        for( final IMarker marker : markers )
        {
          try
          {
            sb.append( marker.getAttribute( IMarker.MESSAGE ) );
          }
          catch( final CoreException e )
          {
            e.printStackTrace();
            // should never happen
          }
        }
      }

      return sb.length() == 0 ? null : sb.toString();
    }

    return null;
  }

  public Color getForeground( final Object element, int columnIndex )
  {
    return null;
  }

  public final String getDeviderTyp( final IProfilPoint point )
  {
    final IContentProvider contentProvider = m_viewer.getContentProvider();
    if( contentProvider instanceof ProfilContentProvider )
    {
      final IProfil profil = ((ProfilContentProvider) contentProvider).getProfil();
      final IProfilPointMarker[] markers = profil == null ? new IProfilPointMarker[0] : profil.getPointMarkerFor( point );
      return markers.length > 0 ? markers[0].getMarkerId() : null;
    }
    else
      return null;
  }

  public Color getBackground( final Object element, final int columnIndex )
  {
    final IContentProvider contentProvider = m_viewer.getContentProvider();
    if( contentProvider instanceof ProfilContentProvider )
    {
      final Map<IProfilPoint, Collection<IMarker>> markerIndex = ((ProfilContentProvider) contentProvider).getMarkerIndex();
      final Collection<IMarker> markers = markerIndex.get( element );

      final Integer severity = worstOf( markers );
      if( severity == null )
        return null;

      if( IMarker.SEVERITY_ERROR == severity )
        return m_colorError;
      if( IMarker.SEVERITY_WARNING == severity )
        return m_colorWarning;
    }

    return null;
  }
}