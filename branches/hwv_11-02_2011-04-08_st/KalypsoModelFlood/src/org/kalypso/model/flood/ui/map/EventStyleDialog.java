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
package org.kalypso.model.flood.ui.map;

import java.math.BigDecimal;
import java.util.List;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.model.flood.i18n.Messages;
import org.kalypso.ui.editor.sldEditor.PolygonColorMapEditorComposite;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree_impl.graphics.sld.PolygonColorMap;

/**
 * @author Thomas jung
 * 
 */
public class EventStyleDialog extends TitleAreaDialog
{
  private final PolygonColorMap m_colorMap;

  private PolygonColorMapEditorComposite m_polygonComponent;

  private final BigDecimal m_min;

  private final BigDecimal m_max;

  public EventStyleDialog( final Shell shell, final PolygonColorMap colorMap, final BigDecimal min, final BigDecimal max )
  {
    super( shell );
    m_colorMap = colorMap;
    m_min = min;
    m_max = max;
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  @Override
  protected Control createDialogArea( Composite parent )
  {
    setTitle( Messages.getString("org.kalypso.model.flood.ui.map.EventStyleDialog.0") ); //$NON-NLS-1$
    setMessage( Messages.getString("org.kalypso.model.flood.ui.map.EventStyleDialog.1") ); //$NON-NLS-1$

    parent.getShell().setText( Messages.getString("org.kalypso.model.flood.ui.map.EventStyleDialog.2") ); //$NON-NLS-1$

    final Composite panel = (Composite) super.createDialogArea( parent );

    PolygonColorMapEntry toEntry;
    PolygonColorMapEntry fromEntry;

    final PolygonColorMapEntry[] colorMapList = m_colorMap.getColorMap();
    if( colorMapList.length > 0 )
    {
      fromEntry = colorMapList[0];
      toEntry = colorMapList[colorMapList.length - 1];
    }
    else
    {
      fromEntry = null;
      toEntry = null;
    }

    m_polygonComponent = new PolygonColorMapEditorComposite( panel, SWT.NONE, fromEntry, toEntry, m_min, m_max )
    {
      @Override
      protected void colorMapChanged( )
      {
      }
    };
    return panel;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#okPressed()
   */
  @Override
  protected void okPressed( )
  {
    final List<PolygonColorMapEntry> colorMap = m_polygonComponent.getColorMap();
    m_colorMap.replaceColorMap( colorMap );

    super.okPressed();
  }
}
