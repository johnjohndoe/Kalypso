package org.kalypsodeegree_impl.gml.schema.vistors;

import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.kalypsodeegree.model.feature.FeatureAssociationTypeProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;

public class SubstitutionGroupRegistrator implements GMLSchemaVisitor
{

    private final FeatureType m_type;
    private String m_substitutionGroup;
    private final List m_parsedUrls =new ArrayList();

    public SubstitutionGroupRegistrator(FeatureType type)
    {
        m_type = type;        
        m_substitutionGroup = m_type.getSubstitutionGroup();
    }

    public void visit(GMLSchema schema)
    {
        if(m_substitutionGroup==null || m_substitutionGroup.length()<1)
            return;
        FeatureType[] featureTypes = schema.getFeatureTypes();
        for (int i = 0; i < featureTypes.length; i++)
            visit(featureTypes[i]);
        GMLSchema[] importedSchemas = schema.getImportedSchemas();
        for (int i = 0; i < importedSchemas.length; i++)
        {
            GMLSchema schema2 = importedSchemas[i];
            URL url = schema2.getUrl();
            if(!m_parsedUrls.contains(url))
                {schema2.accept(this);
                 m_parsedUrls.add(url);
                }
        }
    }

    private void visit(FeatureType ft)
    {
        if(ft==m_type)
            return;
        FeatureTypeProperty[] properties = ft.getProperties();
        for (int i = 0; i < properties.length; i++)
            visit(properties[i]);
    }

    private void visit(FeatureTypeProperty ftp)
    {
        if(ftp instanceof FeatureAssociationTypeProperty)
        {
            FeatureAssociationTypeProperty fatp=(FeatureAssociationTypeProperty) ftp;
            FeatureType associationFeatureType = fatp.getAssociationFeatureType();
            if(associationFeatureType == null )
                return;
            if(m_substitutionGroup.equals((associationFeatureType.getNamespace()+":"+associationFeatureType.getName())))
                    fatp.registerSubstitution(m_type);
        }
    }
}
