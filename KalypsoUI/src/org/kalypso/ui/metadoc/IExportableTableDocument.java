package org.kalypso.ui.metadoc;

/**
 * IExportableTableDocument
 * 
 * @author schlienger
 */
public interface IExportableTableDocument extends IExportableDocument
{
  /**
   * When true, only the selected rows should be exported
   * 
   * @param flag
   */
  public void setOnlySelectedRows( final boolean flag );
}
