package org.kalypso.java.swing.table;

import java.awt.Toolkit;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.text.NumberFormat;
import java.util.StringTokenizer;

import javax.swing.AbstractAction;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.KeyStroke;

/**
 * ExcelAdapter enables Copy-Paste Clipboard functionality on JTables. The
 * clipboard data format used by the adapter is compatible with the clipboard
 * format used by Excel. This provides for clipboard interoperability between
 * enabled JTables and Excel.
 * <p>
 * This code is adapted from the Online Article at
 * {@link http://www.javaworld.com/javaworld/javatips/jw-javatip77.html}
 */
public final class ExcelClipboardAdapter
{
  public final static String CMD_COPY = "COPY";
  public final static String CMD_PASTE = "PASTE";

  protected final JTable m_table;
  protected final NumberFormat m_nf;

  private final KeyStroke m_ksCopy;
  private final KeyStroke m_ksPaste;
  private CopyAction m_copyAction;
  private PasteAction m_pasteAction;

  /**
   * The Excel Adapter is constructed with a JTable on which it enables
   * Copy-Paste and acts as a Clipboard listener.
   */
  public ExcelClipboardAdapter( final JTable table, final NumberFormat nf )
  {
    m_table = table;
    m_nf = nf;

    m_ksCopy = KeyStroke.getKeyStroke( KeyEvent.VK_C, ActionEvent.CTRL_MASK,
        false );

    m_ksPaste = KeyStroke.getKeyStroke( KeyEvent.VK_V, ActionEvent.CTRL_MASK,
        false );

    m_table.getInputMap().put( m_ksCopy, CMD_COPY );
    m_copyAction = new CopyAction();
    m_table.getActionMap().put( CMD_COPY, m_copyAction );
    m_table.getInputMap().put( m_ksPaste, CMD_PASTE );
    m_pasteAction = new PasteAction();
    m_table.getActionMap().put( CMD_PASTE, m_pasteAction );
  }

  public void dispose( )
  {
    m_table.getInputMap().remove( m_ksCopy );
    m_table.getActionMap().remove( CMD_COPY );
    m_table.getInputMap().remove( m_ksPaste );
    m_table.getActionMap().remove( CMD_PASTE );
  }

  public CopyAction getCopyAction( )
  {
    return m_copyAction;
  }
  
  public PasteAction getPasteAction( )
  {
    return m_pasteAction;
  }
  
  private class CopyAction extends AbstractAction
  {
    public CopyAction()
    {
      super( "Kopieren" );
    }
    
    public void actionPerformed( ActionEvent e )
    {
      // Check to ensure we have selected only a contiguous block of cells
      int numcols = m_table.getSelectedColumnCount();
      int numrows = m_table.getSelectedRowCount();
      int[] rowsselected = m_table.getSelectedRows();
      int[] colsselected = m_table.getSelectedColumns();
      if( !((numrows - 1 == rowsselected[rowsselected.length - 1]
          - rowsselected[0] && numrows == rowsselected.length) && (numcols - 1 == colsselected[colsselected.length - 1]
          - colsselected[0] && numcols == colsselected.length)) )
      {
        JOptionPane.showMessageDialog( null, "Invalid Copy Selection",
            "Invalid Copy Selection", JOptionPane.ERROR_MESSAGE );
        return;
      }

      final StringBuffer sbf = new StringBuffer();
      for( int i = 0; i < numrows; i++ )
      {
        for( int j = 0; j < numcols; j++ )
        {
          final Object value = m_table.getValueAt( rowsselected[i],
              colsselected[j] );
          sbf.append( m_nf.format( value ) );

          if( j < numcols - 1 )
            sbf.append( "\t" );
        }

        sbf.append( "\n" );
      }

      final StringSelection stsel = new StringSelection( sbf.toString() );
      Toolkit.getDefaultToolkit().getSystemClipboard().setContents( stsel,
          stsel );
    }
  }

  private class PasteAction extends AbstractAction
  {
    public PasteAction()
    {
      super( "Hinzufügen" );
    }

    public void actionPerformed( ActionEvent e )
    {
      int startRow = (m_table.getSelectedRows())[0];
      int startCol = (m_table.getSelectedColumns())[0];
      try
      {
        String trstring = (String) (Toolkit.getDefaultToolkit()
            .getSystemClipboard().getContents( this )
            .getTransferData( DataFlavor.stringFlavor ));

        final StringTokenizer st1 = new StringTokenizer( trstring, "\n" );

        for( int i = 0; st1.hasMoreTokens(); i++ )
        {
          final String rowstring = st1.nextToken();

          final StringTokenizer st2 = new StringTokenizer( rowstring, "\t" );
          for( int j = 0; st2.hasMoreTokens(); j++ )
          {
            final Object value = m_nf.parseObject( st2.nextToken() );
            if( startRow + i < m_table.getRowCount()
                && startCol + j < m_table.getColumnCount() )
              m_table.setValueAt( value, startRow + i, startCol + j );
          }
        }
      }
      catch( Exception ex )
      {
        ex.printStackTrace();
      }
      finally
      {
        m_table.repaint();
      }
    }
  }
}