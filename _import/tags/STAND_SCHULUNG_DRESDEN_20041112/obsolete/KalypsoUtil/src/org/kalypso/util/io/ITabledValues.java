package org.kalypso.util.io;

/**
 * @author schlienger
 */
public interface ITabledValues
{
  /**
   * Returns the number of lines fetched from the CSV-File.
   */
  public int getLines();

  /**
   * Returns the item at the given position in the CSV-File.
   */
  public String getItem( final int row, final int col );
  
  /**
   * Sets the element in the mem representation of the CSV file
   */
  public void setItem( final int row, final int col, final String str );
}