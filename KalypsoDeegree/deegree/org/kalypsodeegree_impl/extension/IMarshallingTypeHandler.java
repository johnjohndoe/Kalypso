package org.kalypsodeegree_impl.extension;

import java.net.URL;
import java.text.ParseException;

import org.kalypso.contribs.java.net.IUrlResolver;
import org.w3c.dom.Node;

/**
 * @author belger
 */
public interface IMarshallingTypeHandler extends ITypeHandler
{
  /**
   * Serialize object to xml node
   * 
   * @param node
   *          serialize the object into this node
   * @param context
   *          use this context for relative url
   * @param object
   *          object to serialize, it must be instanceof {@link #getClassName()}.
   */
  public void marshall( final Object object, final Node node, final URL context ) throws TypeRegistryException;

  /**
   * creates an object of type {@link #getClassName()}from node.
   * 
   * @param node
   *          unmnarshall this node to an object of type {@link #getClassName()}
   * @param context
   *          use this context for relative url
   */
  public Object unmarshall( final Node node, final URL context, final IUrlResolver urlResolver )
      throws TypeRegistryException;

  /** Ein Kurzname des behandelten Typ, wird z.B: für Beschriftungen benutzt */
  public String getShortname();

  public Object cloneObject( final Object objectToClone ) throws CloneNotSupportedException;
  
  /** Creeates an instance of my type from a string */
  public Object parseType( final String text ) throws ParseException;
}