package org.deegree_impl.gml;

import org.deegree.gml.GMLException;
import org.deegree.gml.GMLProperty;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.extension.ITypeHandler;
import org.deegree_impl.extension.TypeRegistryException;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author belger
 */
public class GMLCustomProperty_Impl extends GMLProperty_Impl
{
  public GMLCustomProperty_Impl( final FeatureTypeProperty ftp, final Element element )
  {
    super( ftp, element );
  }

  public static GMLProperty createGMLProperty( final Document doc, final FeatureTypeProperty ftp,
      final Object customObject ) throws GMLException
  {
    try
    {
    final Element element = doc.createElement( ftp.getName() );

    // marshalling
    final ITypeHandler typeHandler = TypeRegistrySingleton.getTypeRegistry()
        .getTypeHandlerForClassName( ftp.getType() );
    typeHandler.marshall( customObject, element );

    GMLCustomProperty_Impl gmlProp = new GMLCustomProperty_Impl( ftp, element );

    Debug.debugMethodEnd();
    return gmlProp;
    }
    catch( final Exception e )
    {
      throw new GMLException( e.getLocalizedMessage() );
    }
  }

}