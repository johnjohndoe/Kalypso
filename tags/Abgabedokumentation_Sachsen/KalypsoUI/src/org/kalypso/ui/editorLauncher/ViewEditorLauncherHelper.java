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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.part.FileEditorInput;
import org.kalypso.eclipse.core.resources.FileFilterVisitor;

/**
 * Helper-Klasse für die Template-Auswahl
 * 
 * @author belger
 */
public class ViewEditorLauncherHelper
{
  private ViewEditorLauncherHelper()
  {
  // never instantiate this class
  }

  public static void showTemplateDialog( final IPath filePath, final FileFilter fileFilter,
      final IDefaultTemplateLauncher[] defaultTemplates )
  {
    final IWorkspace workspace = ResourcesPlugin.getWorkspace();
    final IWorkspaceRoot root = workspace.getRoot();
    final IFile file = root.getFileForLocation( filePath );
    if( file == null )
      return;

    // vorhandene Vorlagen im Verzeichnis finden
    final IContainer folder = file.getParent();

    // vorhandene Vorlagen im Verzeichnis finden
    final FileFilterVisitor visitor = new FileFilterVisitor( fileFilter );

    try
    {
      folder.accept( visitor, IResource.DEPTH_ONE, false );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

    final ArrayList allTemplates = new ArrayList();

    // virtuelle Vorlagen hinzufügen
    final Map defaultTemplateMap = new HashMap();
    for( int i = 0; i < defaultTemplates.length; i++ )
    {
      // pseudo file erzeugen, damit der LabelProvider was schönes erzeugt
      final IDefaultTemplateLauncher def = defaultTemplates[i];
      final IFile defaultPseudoFile = folder.getFile( new Path( def.getFilename() ) );

      defaultTemplateMap.put( defaultPseudoFile, def );
    }
    allTemplates.addAll( defaultTemplateMap.keySet() );

    // reale Vorlagen hinzufügen
    final IFile[] realTemplates = visitor.getFiles();
    for( int i = 0; i < realTemplates.length; i++ )
      allTemplates.add( realTemplates[i] );

    final IWorkbench workbench = PlatformUI.getWorkbench();
    final Shell shell = workbench.getActiveWorkbenchWindow().getShell();

    final ElementListSelectionDialog dialog = new ElementListSelectionDialog( shell,
        new WorkbenchLabelProvider() );
    dialog.setElements( allTemplates.toArray() );
    dialog.setBlockOnOpen( true );
    dialog.setEmptyListMessage( "Es konnten keine Ansichten für diesen Dateityp ermittelt werden." );
    dialog
        .setMessage( "Diese Datei ist keine Ansichtsvorlage.\nSie können eine vorhandene Ansicht auswählen\noder eine Standardansicht für die enthaltenen Daten generieren." );
    dialog.setMultipleSelection( true );
    dialog.setTitle( "Ansichtsauswahl" );
    dialog.setEmptySelectionMessage( "Wählen Sie mindestens eine Vorlage." );
    dialog.setStatusLineAboveButtons( true );

    // klappt nicht?
    dialog.setInitialSelections( allTemplates.toArray() );

    if( dialog.open() == Window.OK )
    {
      final Object[] templates = dialog.getResult();

      // falls eine ausgewählt wurde, die Vorlage öffnen
      final IEditorRegistry editorRegistry = workbench.getEditorRegistry();
      for( int i = 0; i < templates.length; i++ )
      {
        final IFile template = (IFile)templates[i];

        // wars ein Default? dann extra behandeln
        IEditorInput input = null;
        IEditorDescriptor editorDescription = null;

        final IDefaultTemplateLauncher defaultTemplate = (IDefaultTemplateLauncher)defaultTemplateMap
            .get( template );
        if( defaultTemplate != null )
        {
          editorDescription = defaultTemplate.getEditor();
          input = defaultTemplate.createInput( file );
        }
        else
        {
          editorDescription = editorRegistry.getDefaultEditor( template.getName() );
          input = new FileEditorInput( template );
        }

        if( input != null && editorDescription != null )
        {
          final IWorkbenchPage activePage = workbench.getActiveWorkbenchWindow().getActivePage();
          try
          {
            activePage.openEditor( input, editorDescription.getId(), true );
          }
          catch( final PartInitException e1 )
          {
            e1.printStackTrace();
          }
        }
      }
    }
  }
}