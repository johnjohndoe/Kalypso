/*--------------- Kalypso-Header --------------------------------------------------------------------

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

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editorLauncher;

import org.eclipse.core.resources.IFile;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditor;

/**
 * Launcher, um ein GML im Baum (GmlEditor) anzusehen.
 * 
 * @author belger
 */
public class GmlEditorTemplateLauncher implements IDefaultTemplateLauncher
{
  public GmlEditorTemplateLauncher()
  {
    super();
  }

  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#getFilename()
   */
  public String getFilename()
  {
    return "<Standard Baumansicht>";
  }

  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#getEditor()
   */
  public IEditorDescriptor getEditor()
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IEditorRegistry editorRegistry = workbench.getEditorRegistry();
    return editorRegistry.findEditor( "org.kalypso.ui.editor.GMLEditor" );
  }

  /**
   * @see org.kalypso.ui.editorLauncher.IDefaultTemplateLauncher#createInput(org.eclipse.core.resources.IFile)
   */
  public IEditorInput createInput( final IFile file )
  {
    // todo: determine linktype by extension of file

    return new GMLEditor.GmlEditorInput( "gml", file );
  }
}
