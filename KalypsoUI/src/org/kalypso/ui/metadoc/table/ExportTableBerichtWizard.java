package org.kalypso.ui.metadoc.table;

import org.kalypso.services.proxy.DocBean;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.metadoc.ExportBerichtWizard;
import org.kalypso.ui.metadoc.IExportableDocument;
import org.kalypso.ui.metadoc.IExportableTableDocument;

/**
 * @author belger
 */
public class ExportTableBerichtWizard extends ExportBerichtWizard
{
  private ExportTableOptionsPage m_optionPage;

  public ExportTableBerichtWizard(
      final IExportableDocument document2export, final DocBean doc )
  {
    super( document2export, doc );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  public void addPages( )
  {
    super.addPages();

    m_optionPage = new ExportTableOptionsPage( "optionPage", "Export Otionen",
        ImageProvider.IMAGE_UTIL_BERICHT_WIZ );

    addPage( m_optionPage );
  }

  public boolean performFinish( )
  {
    if( m_document2export instanceof IExportableTableDocument )
      ((IExportableTableDocument) m_document2export).setOnlySelectedRows( m_optionPage.getOnlySelected() );

    m_optionPage.saveWidgetValues();

    return super.performFinish();
  }
}