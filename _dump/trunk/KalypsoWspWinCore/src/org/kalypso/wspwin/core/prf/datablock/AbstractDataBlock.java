package org.kalypso.wspwin.core.prf.datablock;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;

/**
 * @author KimWerner
 */
public abstract class AbstractDataBlock implements IDataBlock
{
  protected final DataBlockHeader m_dataBlockHeader;



  public AbstractDataBlock( final DataBlockHeader dbh )
  {
    m_dataBlockHeader = dbh;
  }
  public DataBlockHeader getDataBlockHeader( )
  {
    return m_dataBlockHeader;
  }
  public abstract void readFromReader( final int count, final BufferedReader reader ) throws IOException;

  public abstract void printToPrinter( final PrintWriter pw ) throws IOException;
  public static final String formatDouble( final double d )
  {
    return String.format( Locale.US, " 0 %12.4f", new Object[] { new Double( d ) } );
  }
  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#getFirstLine()
   */
  public String getFirstLine( )
  {
        return m_dataBlockHeader.getFirstLine();
  }
  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#getSecondLine()
   */
  public String getSecondLine( )
  {
    return m_dataBlockHeader.getSecondLine();
  }
  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#getThirdLine()
   */
  public String getThirdLine( )
  {
    return m_dataBlockHeader.getThirdLine();
  }
  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#setSecondLine(java.lang.String)
   */
  public void setSecondLine( String text )
  {
    m_dataBlockHeader.setSecondLine(text);
    
  }
  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#setThirdLine(java.lang.String)
   */
  public void setThirdLine( String text )
  {
    m_dataBlockHeader.setThirdLine(text);
  }
  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#setFirstLine(java.lang.String)
   */
  public void setFirstLine( String text )
  {
    m_dataBlockHeader.setFirstLine(text);
    
  }

  
}