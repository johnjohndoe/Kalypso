package org.kalypsodeegree_impl.gml.schema.vistors;

import java.util.ArrayList;
import java.util.List;

import org.kalypsodeegree_impl.gml.schema.GMLSchema;

/**
 * 
 * 
 * visitor that guides inner visitor through schema hierarchy
 * 
 * @author thuel
 */
public class RecursiveSchemaVisitor implements GMLSchemaVisitor
{

  private final GMLSchemaVisitor m_visitor;

  public RecursiveSchemaVisitor( final GMLSchemaVisitor visitor )
  {
    m_visitor = visitor;
  }

  private final List m_parsedSchemas = new ArrayList();

  public boolean visit( final GMLSchema schema )
  {
    if( m_parsedSchemas.contains( schema ) )
      return false;
    m_parsedSchemas.add( schema );
    if( m_visitor.visit( schema ) )
    {
      final GMLSchema[] importedSchemas = schema.getImportedSchemas();
      for( int i = 0; i < importedSchemas.length; i++ )
      {
        GMLSchema importedSchema = importedSchemas[i];
        visit( importedSchema );
      }
    }
    return false;
  }
}