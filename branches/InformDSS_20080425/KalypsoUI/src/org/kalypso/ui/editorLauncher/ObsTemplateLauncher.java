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

import java.io.FileFilter;

import org.apache.commons.io.filefilter.FileFilterUtils;
import org.apache.commons.io.filefilter.IOFileFilter;
import org.eclipse.core.runtime.IPath;
import org.eclipse.ui.IEditorLauncher;

/**
 * @author belger
 */
public class ObsTemplateLauncher implements IEditorLauncher
{
  /**
   * @see org.eclipse.ui.IEditorLauncher#open(org.eclipse.core.runtime.IPath)
   */
  public void open( final IPath filePath )
  {
    final IOFileFilter ottFilter = FileFilterUtils.suffixFileFilter( ".ott" );
    final IOFileFilter odtFilter = FileFilterUtils.suffixFileFilter( ".odt" );

    final FileFilter filter = FileFilterUtils.orFileFilter( odtFilter, ottFilter );

    final IDefaultTemplateLauncher dlTable = new DefaultObservationEditorLauncher( "<Standard Tabellen Editor>.ott",
        ".ott" );
    final IDefaultTemplateLauncher dlDiag = new DefaultObservationEditorLauncher( "<Standard Diagramm Editor>.odt",
        ".odt" );

    ViewEditorLauncherHelper.showTemplateDialog( filePath, filter, new IDefaultTemplateLauncher[]
    {
        dlTable,
        dlDiag } );
  }
}
