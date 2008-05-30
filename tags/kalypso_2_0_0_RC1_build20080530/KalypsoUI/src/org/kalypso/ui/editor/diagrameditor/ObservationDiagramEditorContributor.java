/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.ui.editor.diagrameditor;

import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.part.EditorActionBarContributor;
import org.kalypso.metadoc.IExportTargetModes;
import org.kalypso.metadoc.ui.ExportAction;
import org.kalypso.metadoc.ui.ExportActionContributor;
import org.kalypso.ui.editor.diagrameditor.actions.DiagramPrintAction;

/**
 * @author schlienger
 */
public class ObservationDiagramEditorContributor extends EditorActionBarContributor
{
  private final DiagramPrintAction m_printAction = new DiagramPrintAction();
  private ExportAction[] m_exportActions = null;

  /**
   * @see org.eclipse.ui.part.EditorActionBarContributor#setActiveEditor(org.eclipse.ui.IEditorPart)
   */
  @Override
  public void setActiveEditor( final IEditorPart targetEditor )
  {
    super.setActiveEditor( targetEditor );

    // export actions
    if( m_exportActions == null )
      m_exportActions = ExportActionContributor.contributeActions( targetEditor,
          "org.kalypso.ui.editor.obsdiageditor.menu/exports", "diagramm", IExportTargetModes.MODE_EXPERT ); //$NON-NLS-1$ //$NON-NLS-2$

    if( m_exportActions != null )
    {
      for( int i = 0; i < m_exportActions.length; i++ )
        m_exportActions[i].setActivePart( targetEditor );
    }
  }
  
  /**
   * @see org.eclipse.ui.part.EditorActionBarContributor#init(org.eclipse.ui.IActionBars)
   */
  @Override
  public void init( final IActionBars bars )
  {
    super.init( bars );

    // overwrite the print action for the diagram editor
    bars.setGlobalActionHandler( ActionFactory.PRINT.getId(), m_printAction );
  }
}
