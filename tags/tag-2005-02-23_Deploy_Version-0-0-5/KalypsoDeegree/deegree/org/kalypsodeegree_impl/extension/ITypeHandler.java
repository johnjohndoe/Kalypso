package org.deegree_impl.extension;

import org.w3c.dom.Node;

/**
 * @author belger
 */
public interface ITypeHandler
{
  /** Der Java-Name, auf den dieser Typ gemapt wird */
  public String getClassName();

  /** Der XML-Typname */
  public String getTypeName();

  /**
   * Serialisiert ein Object von diesem Typ als XML-Node
   * 
   * @param object
   *          Dieses Objekt muss vom Typ {@link #getClassName()}sein.
   */
  public void marshall( final Object object, final Node node ) throws TypeRegistryException;

  /**
   * Erzeugt aus einem XML-Knoten ein Objekt vom Typen {@link #getClassName()}.
   */
  public Object unmarshall( final Node node ) throws TypeRegistryException;

  /** Ein Kurzname des behandelten Typ, wird z.B: für Beschriftungen benutzt */
  public String getShortname();
}