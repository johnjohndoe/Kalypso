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
package org.kalypso.ui.editor.styleeditor.panels;

import org.eclipse.swt.widgets.Composite;

/**
 * 
 * @author kuepfer
 */
public class SpatialOperationPanel extends FilterComboPanel
{

  public static final String EQUALS = "EQUALS"; //$NON-NLS-1$

  public static final String TOUCHES = "TOUCHES"; //$NON-NLS-1$

  public static final String WITHIN = "WITHIN"; //$NON-NLS-1$

  public static final String OVERLAPS = "OVERLAPS"; //$NON-NLS-1$

  public static final String CROSSES = "CROSSES"; //$NON-NLS-1$

  public static final String INTERSECTS = "INTERSECTS"; //$NON-NLS-1$

  public static final String CONTAINS = "CONTAINS"; //$NON-NLS-1$

  public static final String DWITHIN = "DWITHIN"; //$NON-NLS-1$

  public static final String BEYOND = "BEYOND"; //$NON-NLS-1$

  public static final String BBOX = "BBOX"; //$NON-NLS-1$

  public static final String DISJOINT = "DISJOINT"; //$NON-NLS-1$

  public SpatialOperationPanel( Composite parent )
  {
    super( parent );
    items = new String[]
    {
        EQUALS,
        TOUCHES,
        WITHIN,
        OVERLAPS,
        CROSSES,
        INTERSECTS,
        CONTAINS,
        DWITHIN,
        DISJOINT,
        BEYOND,
        BBOX };
    init();
  }

  /**
   * @see org.kalypso.ui.editor.styleeditor.panels.FilterComboPanel#setSelection(int)
   */
  @Override
  public void setSelection( int index )
  {
    selection_index = index;
    comboBox.select( index );

  }

  /**
   * @see org.kalypso.ui.editor.styleeditor.panels.FilterComboPanel#getSelection()
   */
  @Override
  public int getSelection( )
  {
    return selection_index;
  }

}
