package de.kisters.wiski.webdataprovider.common.net;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

/**
 * Log writer class.
 */
public class Logwriter
{
  private FileWriter fw;

  private BufferedWriter bw;

  /**
   * Creates a new <code>Log_writer</code> instance. Creates a new output file
   * with a <code>default</code> name in the execution path.
   */
  public Logwriter( )
  {
    try
    {
      fw = new FileWriter( "output.txt" );
      bw = new BufferedWriter( fw, 200000 );
    }
    catch( IOException ioex )
    {
      System.out.println( ioex );
    }
  }

  /**
   * Creates a new <code>Log_writer</code> instance.
   * 
   * <p>
   * 
   * @param filename
   *          name of the created file
   * @param append
   *          boolean for appending to an existing file
   */
  public Logwriter( String filename, boolean append )
  {
    try
    {
      if( append )
      {
        fw = new FileWriter( filename, true );
        bw = new BufferedWriter( fw );
      }
      else
      {
        fw = new FileWriter( filename );
        bw = new BufferedWriter( fw, 200000 );
      }
    }
    catch( IOException ioex )
    {
      System.out.println( ioex );
    }
  }

  /**
   * 
   * <p>
   * 
   * @param entry
   *          Entry to be saved in the log file
   */
  public void add_entry( String entry )
  {

    String entrybuffer;
    if( entry.charAt( entry.length() - 1 ) == '\n' )
      entrybuffer = entry.substring( 0, (entry.length() - 1) );
    else
      entrybuffer = entry;
    try
    {
      bw.newLine();
      bw.write( entrybuffer );
    }

    catch( IOException ioex )
    {
      System.out.println( ioex );
    }
  }

  /**
   * Save the log file
   */
  public void save_log( )
  {
    try
    {
      bw.close();
    }

    catch( IOException ioex )
    {
      System.out.println( ioex );
    }
  }

}