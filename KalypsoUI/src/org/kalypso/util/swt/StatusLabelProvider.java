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
package org.kalypso.util.swt;

import java.text.DateFormat;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.internal.ide.IDEInternalWorkbenchImages;
import org.kalypso.contribs.eclipse.jface.viewers.DefaultTableViewer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;

/**
 * A label provider showing stati.
 * 
 * @author Gernot Belger
 */
public class StatusLabelProvider extends LabelProvider implements ITableLabelProvider
{
  private final static DateFormat DF = DateFormat.getDateTimeInstance( DateFormat.SHORT, DateFormat.SHORT );
  static
  {
    DF.setTimeZone( KalypsoGisPlugin.getDefault().getDisplayTimeZone() );
  }

  private static final String TIME = "time";

  private static final String MESSAGE = "message";

  private static final String SEVERITY = "severity";

  private final Object[] m_columnProperties;

  public StatusLabelProvider( final Object[] columnProperties )
  {
    m_columnProperties = columnProperties;
  }

  /**
   * Configure a default table viewer, to work with this label provider.<br>
   * Also sets an instance of this class as label provider.
   */
  public static void configureTableViewer( final DefaultTableViewer tableViewer )
  {
    tableViewer.addColumn( SEVERITY, "Art", null, 30, 0, false, SWT.CENTER, false, true );
    tableViewer.addColumn( MESSAGE, "Beschreibung", null, 500, 0, false, SWT.LEFT, true, true );
    tableViewer.addColumn( TIME, "Zeit", null, 150, 0, false, SWT.LEFT, false, true );

    tableViewer.setLabelProvider( new StatusLabelProvider( tableViewer.getColumnProperties() ) );
  }

  /**
   * @see org.kalypso.gml.ui.jface.FeatureWrapperLabelProvider#getColumnImage(java.lang.Object, int)
   */
  @SuppressWarnings("restriction")
  public Image getColumnImage( final Object element, final int columnIndex )
  {
    if( !(element instanceof IStatus) )
      return null;

    final Object columnProperty = m_columnProperties[columnIndex];

    final IStatus status = (IStatus) element;

    if( columnProperty == SEVERITY )
    {
      // Special treatment for cancel: show as warning
      if( status.matches( IStatus.CANCEL ) )
        return StatusComposite.getIDEImage( IDEInternalWorkbenchImages.IMG_OBJS_WARNING_PATH );

      return StatusComposite.getStatusImage( status );
    }

    return null;
  }

  /**
   * @see org.kalypso.gml.ui.jface.FeatureWrapperLabelProvider#getColumnText(java.lang.Object, int)
   */
  public String getColumnText( final Object element, final int columnIndex )
  {
    if( !(element instanceof IStatus) )
      return "";

    final Object columnProperty = m_columnProperties[columnIndex];

    final IStatus status = (IStatus) element;

    if( columnProperty == MESSAGE )
      return status.getMessage();

    if( columnProperty == TIME )
    {
      if( status instanceof IGeoStatus )
        return ((IGeoStatus) status).getTime().toString();
    }

    return "";
  }
}
