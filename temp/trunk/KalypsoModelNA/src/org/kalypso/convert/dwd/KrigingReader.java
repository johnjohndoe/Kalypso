package org.kalypso.convert.dwd;

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Reader;
import java.io.StringWriter;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.deegree.model.feature.Feature;
import org.deegree.model.geometry.GM_Object;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.zml.filters.NOperationFilter;
import org.kalypso.zml.filters.ObjectFactory;
import org.kalypso.zml.filters.OperationFilter;
import org.kalypso.zml.filters.ZmlFilter;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3._1999.xlinkext.SimpleLinkType;

public class KrigingReader
{
    private final String doublePattern = "[0-9\\.]+";
    private final ObjectFactory filterFac = new ObjectFactory();
    private final Marshaller m_marshaller;
    private final Pattern BLOCK = Pattern.compile(".*BLOCK:.+?("
            + doublePattern + ").+?(" + doublePattern + ").+");

    //    BLOCK: 4480066.00 5560799.00 1000000.00 1 1 1
    private final Pattern RELATION = Pattern.compile(".*?(" + doublePattern
            + ") +?(" + doublePattern + ") +?(" + doublePattern + ") +(.+?) *");

    //    4485832.000 5603328.000 0.420 4234
    private final List krigingElements;

    private int m_min = 9999999;

    public KrigingReader(Reader reader) throws IOException, JAXBException
    {
        m_marshaller= filterFac.createMarshaller();
        krigingElements = parse(reader);
        reader.close();
    }

    public String createFilter(final Feature feature, final String geoPropName)
    {
        System.out.println("creatFilter for " + feature.getId());
        final GM_Object geom = (GM_Object) feature.getProperty(geoPropName);
        final List elements = getKrigingElementsFor(geom);
        if (elements.isEmpty())
            throw new InvalidParameterException(
                    "Raster ist zu grob, Keine zuordnung f?r "
                            + feature.getId() + " gefunden.\n");
        return createFilter(elements);
    }

    private String createFilter(List krigingElements)
    {
        if (krigingElements.size() < m_min)
            m_min = krigingElements.size();
        System.out.println(krigingElements.size()
                + " rasterpoints are withing geometry. (min is" + m_min + ")");

        final HashMap map = new HashMap();
        final double n = krigingElements.size();
        // loop elements
        for (Iterator iter = krigingElements.iterator(); iter.hasNext();)
        {
            KrigingElement ke = (KrigingElement) iter.next();
            KrigingRelation[] relations = ke.getRelations();
            // loop relations
            for (int i = 0; i < relations.length; i++)
            {
                KrigingRelation relation = relations[i];
                final String id = relation.getId();
                final double factor = relation.getFactor() / n;
                if (!map.containsKey(id))
                    map.put(id, new KrigingRelation(factor, id));
                else
                {
                    KrigingRelation rel = (KrigingRelation) map.get(id);
                    rel.setFactor(rel.getFactor() + factor);
                }
            }
        }
        final org.w3._1999.xlinkext.ObjectFactory linkFac = new org.w3._1999.xlinkext.ObjectFactory();
        
        try
        {
            NOperationFilter nOperationFilter = filterFac
                    .createNOperationFilter();
            nOperationFilter.setOperator("+");
            List filterList = nOperationFilter.getFilter();
            for (Iterator iter = map.values().iterator(); iter.hasNext();)
            {
                final OperationFilter filter = filterFac
                        .createOperationFilter();
                filterList.add(filter);
                final KrigingRelation rel = (KrigingRelation) iter.next();
                filter.setOperator("*");
                filter.setOperand(Double.toString(rel.getFactor()));
                final ZmlFilter zmlLink = filterFac.createZmlFilter();
                final SimpleLinkType type = linkFac.createSimpleLinkType();
                type.setHref("Ombrometer_"+rel.getId());
                zmlLink.setZml(type);
                filter.setFilter(zmlLink);
                System.out.println(rel.getId() + " " + rel.getFactor());
            }
            StringWriter writer=new StringWriter();
            m_marshaller.marshal(nOperationFilter,writer);
            writer.close();
            System.out.println(writer.toString());
        } catch (JAXBException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e)
        {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return null;
    }

    private List getKrigingElementsFor(GM_Object geom)
    {
        List result = new ArrayList();
        for (Iterator iter = krigingElements.iterator(); iter.hasNext();)
        {
            KrigingElement ke = (KrigingElement) iter.next();
            if (geom.contains(ke.getCenterPoint()))
                result.add(ke);
        }
        return result;
    }

    public List parse(Reader reader)
    {
        final List result = new ArrayList();
        try
        {
            CS_CoordinateSystem srs = ConvenienceCSFactory.getInstance()
                    .getOGCCSByName("EPSG:31467");
            final LineNumberReader lineReader = new LineNumberReader(reader);
            String line = null;
            KrigingElement e = null;
            while ((line = lineReader.readLine()) != null)
            {
                Matcher m1 = BLOCK.matcher(line);
                Matcher m2 = RELATION.matcher(line);
                if (m1.matches())
                {
                    double x = Double.parseDouble(m1.group(1));
                    double y = Double.parseDouble(m1.group(2));
                    e = new KrigingElement(GeometryFactory.createGM_Point(x, y,
                            srs));
                    result.add(e);
                } else if (m2.matches())
                {
                    final double factor = Double.parseDouble(m2.group(3));
                    final String id = m2.group(4);
                    if (e != null)
                        e.addRelation(factor, id);
                }
            }
            lineReader.close();
        } catch (NumberFormatException e1)
        {
            e1.printStackTrace();
        } catch (IOException e1)
        {
            e1.printStackTrace();
        }
        return result;
    }

}