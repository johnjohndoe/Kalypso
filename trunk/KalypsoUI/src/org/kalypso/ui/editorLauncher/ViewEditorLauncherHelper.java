package org.kalypso.ui.editorLauncher;

import java.io.FileFilter;
import java.util.ArrayList;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.part.FileEditorInput;
import org.kalypso.eclipse.core.resources.FileFilterVisitor;

/**
 * Helper-Klasse f�r die Template-Auswahl
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
      final Object[] defaultTemplates )
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

    final IFile[] realTemplates = visitor.getFiles();

    // virtuelle Vorlagen finden

    // einen Dialog mit den m�glichen Vorlagen anzeigen
    final ArrayList allTemplates = new ArrayList();
    for( int i = 0; i < defaultTemplates.length; i++ )
      allTemplates.add( defaultTemplates[i] );
    for( int i = 0; i < realTemplates.length; i++ )
      allTemplates.add( realTemplates[i] );

    final IWorkbench workbench = PlatformUI.getWorkbench();
    final Shell shell = workbench.getActiveWorkbenchWindow().getShell();

    final ElementListSelectionDialog dialog = new ElementListSelectionDialog( shell,
        new WorkbenchLabelProvider() );
    dialog.setElements( allTemplates.toArray() );
    dialog.setBlockOnOpen( true );
    dialog.setEmptyListMessage( "Es konnten keine Ansichten f�r diesen Dateityp ermittelt werden." );
    dialog
        .setMessage( "Diese Datei ist keine Ansichtsvorlage.\nSie k�nnen eine vorhandene Ansicht ausw�hlen\noder eine Standardansicht f�r die enthaltenen Daten generieren." );
    dialog.setMultipleSelection( true );
    dialog.setTitle( "Ansichtsauswahl" );
    dialog.setEmptySelectionMessage( "W�hlen Sie mindestens eine Vorlage." );
    dialog.setStatusLineAboveButtons( true );

    // klappt nicht?
    dialog.setInitialSelections( allTemplates.toArray() );

    if( dialog.open() == Window.OK )
    {
      final Object[] templates = dialog.getResult();

      // falls eine ausgew�hlt wurde, die Vorlage �ffnen
      try
      {
        final IEditorRegistry editorRegistry = workbench.getEditorRegistry();
        for( int i = 0; i < templates.length; i++ )
        {
          final IFile template = (IFile)templates[i];

          // wars ein Default? dann extra behandeln
          IEditorInput input = null;
          IEditorDescriptor editorDescription = null;

          // TODO: etwas mit den defaults machen!
          boolean bFound = false;
          for( int j = 0; j < defaultTemplates.length; j++ )
          {
            if( defaultTemplates[j] == template )
            {
              MessageDialog.openInformation( shell, "Vorlagenansicht",
                  "Die Standardvorlagenansicht ist noch nicht verf�gbar" );

              // TODO mach was!
              bFound = true;
              break;
            }
          }
          if( bFound )
            continue;

          editorDescription = editorRegistry.getDefaultEditor( template.getName() );

          input = new FileEditorInput( template );

          if( input != null && editorDescription != null )
          {
            final IWorkbenchPage activePage = workbench.getActiveWorkbenchWindow().getActivePage();
            activePage.openEditor( input, editorDescription.getId(), true );
          }
        }
      }
      catch( final WorkbenchException e1 )
      {
        e1.printStackTrace();
      }
    }
  }
}