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
package org.kalypso.model.wspm.ui.view.table;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointMarker;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;

/**
 * @author Gernot Belger
 * @author Kim Werner
 */
public class ComponentUiProblemHandler implements IComponentUiHandler
{
  private static final String IMAGE_ERROR = "profilLabelProvider.img.error";

  private static final String IMAGE_WARNING = "profilLabelProvider.img.warning";

  private final ImageRegistry m_imgRegistry = new ImageRegistry();

// private final Color m_colorError;
//
// private final Color m_colorWarning;

  private final IProfil m_profile;

  public ComponentUiProblemHandler( final IProfil profile, final Display display )
  {
    m_profile = profile;

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
// final Display display = m_viewer.getControl().getDisplay();
// m_colorError = display.getSystemColor( SWT.COLOR_RED );
// m_colorWarning = display.getSystemColor( SWT.COLOR_YELLOW );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#createCellEditor(org.eclipse.swt.widgets.Table)
   */
  public CellEditor createCellEditor( final Table table )
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#formatValue(org.kalypso.observation.result.IRecord)
   */
  public Object getValue( final IRecord record )
  {
    return null;
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#getColumnLabel()
   */
  public String getColumnLabel( )
  {
    return "-";
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#getColumnStyle()
   */
  public int getColumnStyle( )
  {
    return SWT.CENTER;
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#getColumnWidth()
   */
  public int getColumnWidth( )
  {
    return 20;
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#getColumnWidthPercent()
   */
  public int getColumnWidthPercent( )
  {
    return -1;
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#getIdentity()
   */
  public String getIdentity( )
  {
    return getClass().getName();
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#getStringRepresentation(org.kalypso.observation.result.IRecord)
   */
  public String getStringRepresentation( final IRecord record )
  {
    // TODO: find problem for record
    final Map<IRecord, Collection<IMarker>> markerIndex = new HashMap<IRecord, Collection<IMarker>>(); // ((ProfilContentProvider)
    // contentProvider).getMarkerIndex();

    final Collection<IMarker> markers = markerIndex.get( record );
    final Integer severity = worstOf( markers );
    final IComponent deviderTyp = getDeviderTyp( record );
    if( severity != null )
    {
      // TODO: das bei 'getImage()
// if( IMarker.SEVERITY_ERROR == severity )
// return m_imgRegistry.get( IMAGE_ERROR );
// if( IMarker.SEVERITY_WARNING == severity )
// return m_imgRegistry.get( IMAGE_WARNING );

      // TODO: text des schlimmsten markers, bzw. nix: dann tooltip stattdessen
      return "" + severity;
    }

    return "";
  }

  // TODO: add to interface and use in label provider
  public Image getImage( final IRecord record )
  {
    // TODO: find problem for record
    final Map<IRecord, Collection<IMarker>> markerIndex = new HashMap<IRecord, Collection<IMarker>>(); // ((ProfilContentProvider)
    // contentProvider).getMarkerIndex();

    final Collection<IMarker> markers = markerIndex.get( record );
    final Integer severity = worstOf( markers );
    if( severity != null )
    {
      if( IMarker.SEVERITY_ERROR == severity )
        return m_imgRegistry.get( IMAGE_ERROR );
      if( IMarker.SEVERITY_WARNING == severity )
        return m_imgRegistry.get( IMAGE_WARNING );
    }

    final IComponent deviderTyp = getDeviderTyp( record );
    if( deviderTyp == null )
      return null;

    return m_imgRegistry.get( deviderTyp.getId() );
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#isEditable()
   */
  public boolean isEditable( )
  {
    return false;
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#isMoveable()
   */
  public boolean isMoveable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#isResizeable()
   */
  public boolean isResizeable( )
  {
    return false;
  }

  /**
   * @see org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler#setValue(org.kalypso.observation.result.IRecord,
   *      java.lang.Object)
   */
  public void setValue( final IRecord record, final Object value )
  {
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

  public final IComponent getDeviderTyp( final IRecord point )
  {
    final IProfilPointMarker[] markers = m_profile.getPointMarkerFor( point );
    if( markers == null )
      return null;

    return markers.length > 0 ? markers[0].getId() : null;
  }
}
