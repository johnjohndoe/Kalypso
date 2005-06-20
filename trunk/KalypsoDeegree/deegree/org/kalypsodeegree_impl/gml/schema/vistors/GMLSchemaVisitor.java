package org.kalypsodeegree_impl.gml.schema.vistors;

import org.kalypsodeegree_impl.gml.schema.GMLSchema;

public interface GMLSchemaVisitor
{
  void visit( GMLSchema schema );
}
