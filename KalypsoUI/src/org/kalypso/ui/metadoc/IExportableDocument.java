package org.kalypso.ui.metadoc;

import java.io.Writer;

/**
 * IExportableDocument
 * 
 * @author schlienger
 */
public interface IExportableDocument
{
  /**
   * Exports the document using the writer.
   * 
   * @param writer
   */
  public void exportDocument( final Writer writer );
}
