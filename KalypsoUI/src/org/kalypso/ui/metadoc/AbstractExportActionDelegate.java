package org.kalypso.ui.metadoc;

import java.lang.reflect.Constructor;

import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.services.proxy.DocBean;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorActionDelegate;
import org.kalypso.ui.metadoc.util.MetadocServiceWrapper;

/**
 * AbstractExportActionDelegate
 * 
 * @author schlienger
 */
public abstract class AbstractExportActionDelegate extends AbstractEditorActionDelegate
{
  private final static Class[] CONS_SIGN = { IExportableDocument.class, DocBean.class };
  private MetadocServiceWrapper m_metadocService;

  /**
   * Performs the export action
   * 
   * @param wizardClass
   * @param expDoc
   * @param shell
   */
  public void runExportAction( final Class wizardClass, final IExportableDocument expDoc, final Shell shell )
  {
    try
    {
      final String username = System.getProperty( "user.name" );
      
      m_metadocService = new MetadocServiceWrapper( expDoc.getDocumentExtension(), username );
      final DocBean doc = m_metadocService.getDoc();

      final Constructor constructor = wizardClass.getConstructor( CONS_SIGN );
      final Wizard exportWizard = (Wizard) constructor.newInstance( new Object[] {expDoc, doc} ); 

      final WizardDialog dialog = new WizardDialog( shell, exportWizard );
      if( dialog.open() == Window.OK )
        m_metadocService.commitData(  );
      else
        m_metadocService.cancelData(  );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      ErrorDialog.openError( shell, "Fehler",
          "Bericht konnte nicht exportiert werden", KalypsoGisPlugin.createErrorStatus("", e ) );
    }
  }
}