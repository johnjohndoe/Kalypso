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

import org.eclipse.core.resources.IMarker;
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
import org.kalypso.model.wspm.core.profil.MarkerIndex;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.ogc.gml.om.table.handlers.IComponentUiHandler;

/**
 * TODO: show marker text as tooltip<br>
 * TODO: open dialog that shows all markers if user clicks on marker (use cell-editor)
 * 
 * @author Gernot Belger
 * @author Kim Werner
 */
public class ComponentUiProblemHandler implements IComponentUiHandler
{
  private static final String IMAGE_ERROR = "profilLabelProvider.img.error"; //$NON-NLS-1$

  private static final String IMAGE_WARNING = "profilLabelProvider.img.warning"; //$NON-NLS-1$

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
    return "-"; //$NON-NLS-1$
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
    return ""; //$NON-NLS-1$
  }

  public Image getImage( final IRecord record )
  {
    final MarkerIndex markerIndex = m_profile.getProblemMarker();
    if( markerIndex == null )
      return null;

    final IMarker[] markers = markerIndex.get( record );
    final IMarker worst = MarkerUtils.worstOf( markers );
    if( worst != null )
    {
      final int severity = MarkerUtils.getSeverity( worst );
      if( IMarker.SEVERITY_ERROR == severity )
        return m_imgRegistry.get( IMAGE_ERROR );
      if( IMarker.SEVERITY_WARNING == severity )
        return m_imgRegistry.get( IMAGE_WARNING );
// if( IMarker.SEVERITY_INFO == severity )
// return m_imgRegistry.get( IMAGE_INFO );
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
    // TODO: set to true and implement editor
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

  public final IComponent getDeviderTyp( final IRecord point )
  {
    final IProfilPointMarker[] markers = m_profile.getPointMarkerFor( point );
    if( markers == null )
      return null;

    return markers.length > 0 ? markers[0].getId() : null;
  }
}
