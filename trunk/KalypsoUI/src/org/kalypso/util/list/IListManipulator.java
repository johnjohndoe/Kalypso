package org.kalypso.util.list;

/**
 * @author bce
 */
public interface IListManipulator
{
  public void moveElementDown( final Object element );
  
  public void moveElementUp( final Object element );
  
  public void removeElement( final Object element );
  
  public void addElement( final Object elementBefore );
}
