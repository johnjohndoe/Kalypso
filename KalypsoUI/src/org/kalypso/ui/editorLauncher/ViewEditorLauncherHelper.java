package org.kalypso.ui.editorLauncher;

import java.io.FileFilter;
import java.util.ArrayList;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
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

  public static void showTemplateDialog( final IContainer folder, final FileFilter fileFilter,
      final IFile[] defaultTemplates )
  {
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

    // einen Dialog mit den möglichen Vorlagen anzeigen
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
              "Die Standardvorlagenansicht ist noch nicht verfügbar" );

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