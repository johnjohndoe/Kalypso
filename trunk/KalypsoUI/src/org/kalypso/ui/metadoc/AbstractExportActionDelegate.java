package org.kalypso.ui.metadoc;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.rmi.RemoteException;

import javax.xml.rpc.ServiceException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.services.ProxyFactory;
import org.kalypso.services.proxy.DocBean;
import org.kalypso.services.proxy.IMetaDocService;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.obstableeditor.actions.AbstractEditorActionDelegate;

/**
 * AbstractExportActionDelegate
 * 
 * @author schlienger
 */
public abstract class AbstractExportActionDelegate extends AbstractEditorActionDelegate
{
  private final static Class[] CONS_SIGN = { IExportableDocument.class, DocBean.class };
  private IMetaDocService m_service;

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
      final DocBean doc = prepareService( expDoc.getDocumentExtension() );

      final Constructor constructor = wizardClass.getConstructor( CONS_SIGN );
      final Wizard exportWizard = (Wizard) constructor.newInstance( new Object[] {expDoc, doc} ); 

      final WizardDialog dialog = new WizardDialog( shell, exportWizard );
      if( dialog.open() == Window.OK )
        saveData( doc );
      else
      {
        cancelData( doc );
      }
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      ErrorDialog.openError( shell, "Fehler",
          "Bericht konnte nicht exportiert werden", KalypsoGisPlugin.createErrorStatus("", e ) );
    }
  }

  protected void cancelData( final DocBean doc ) throws CoreException
  {
    try
    {
      m_service.rollbackNewDocument( doc );
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Löschen der Berichtsvorlage auf dem Server gescheitert", e ) );
    }
  }

  protected void saveData( final DocBean bean ) throws CoreException
  {
    try
    {
      m_service.commitNewDocument( bean );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Berichtsablage gescheitert", e ) );
    }
  }

  protected DocBean prepareService( final String fileExtension )
      throws CoreException
  {
    try
    {
      final ProxyFactory serviceProxyFactory = KalypsoGisPlugin.getDefault()
          .getServiceProxyFactory();
      m_service = (IMetaDocService) serviceProxyFactory.getProxy(
          "Kalypso_MetaDocService", ClassUtilities
              .getOnlyClassName( IMetaDocService.class ) );

      return m_service.prepareNewDocument( fileExtension );
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Aufruf des Rechendienstes", e ) );
    }
    catch( final ServiceException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Rechendienst konnte nicht initialisiert werden", e ) );
    }
  }
}