package com.bce.datacenter.kalypso.dcbrowser;

import com.bce.datacenter.kalypso.DataObject;
import com.bce.datacenter.kalypso.Level;
import com.bce.datacenter.kalypso.Channel;
import com.bce.datacenter.kalypso.Timeserie;
import java.util.List;
import java.util.ArrayList;
import javax.swing.table.DefaultTableModel;

/**
 * Table Model for displaying Level's contents
 *
 * @author schlienger
 */
public class DCChannelTableModel extends DefaultTableModel {
	private final static String[] COLUMN_NAMES =
		{ "Name", "Beginn", "Ende", "Key" };
	private final static Class[] COLUMN_CLASSES =
		{ String.class, String.class, String.class, String.class };
	private Channel m_currentChannel = null;
	private List m_myTimeSeries = null;
	/**
	 * Constructor
	 *
	 * @param level one level of the hierarchy
	 */
	public DCChannelTableModel(Channel channel) {
		super(1, 4);

		setCurrentChannel(channel);
	}

	public Timeserie getTimeserie(int row) {
		if (m_myTimeSeries.size() > 0) {
			//System.out.println("Timeserie: "+m_myTimeSeries+" TimeseriesSize: "+m_myTimeSeries.size());
			return (Timeserie) m_myTimeSeries.get(row);
		} else {
			return null;
		}
	}

	public DCChannelTableModel(Level level) {
		super(1, 4);

		setCurrentLevel(level);
	}

	/**
	 * @see javax.swing.table.TableModel#isCellEditable(int, int)
	 */
	public boolean isCellEditable(int row, int column) {
		return false;
	}

	/**
	 * @see javax.swing.table.TableModel#getColumnClass(int)
	 */
	public Class getColumnClass(int column) {
		return COLUMN_CLASSES[column];
	}

	/**
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	public int getColumnCount() {
		return COLUMN_NAMES.length;
	}

	/**
	 * @see javax.swing.table.TableModel#getColumnName(int)
	 */
	public String getColumnName(int column) {
		return COLUMN_NAMES[column];
	}

	/**
	 * updates table contents
	 *
	 * @param level
	 *
	 * @throws IllegalArgumentException if level is null
	 */
	public void setCurrentLevel(Level level) {
		if (level == null)
			throw new IllegalArgumentException("Level is null");

		m_currentChannel = null;
		m_myTimeSeries = new ArrayList();
		fireTableDataChanged();
	}

	/**
	 * updates table contents
	 *
	 * @param level
	 *
	 * @throws IllegalArgumentException if level is null
	 */
	public void setCurrentChannel(Channel channel) {
		if (channel == null)
			throw new IllegalArgumentException("Channel is null");

		m_currentChannel = channel;
		if (m_currentChannel == null)
			m_myTimeSeries = new ArrayList();
		else
			m_myTimeSeries = m_currentChannel.getTimeseries();

		fireTableDataChanged();
	}

	/**
	 * Returns the current level
	 *
	 * @return
	 */
	public Channel getCurrentChannel() {
		return m_currentChannel;
	}

	/**
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	public int getRowCount() {
		if (m_currentChannel == null)
			return 0;
		else
			return m_myTimeSeries.size();
	}

	/**
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	public Object getValueAt(int row, int column) {
		//{ "Name", "Beginn", "Ende", "Key" };
		Timeserie t = getTimeserie(row);

		switch (column) {
			case 0 : // NAME
				return t.getName();
			case 1 : // Beginn
				if (t.getRealBegin() != null)
					return t.getRealBegin().toString();
				else
					return "unknown";
			case 2 : // Ende
				if (t.getRealEnd() != null)
					return t.getRealEnd().toString();
				else
					return "unknown";
			case 3 : // Key
				return t.getDataTableName();
			default :
				break;
		}
		return null;
	}
}
