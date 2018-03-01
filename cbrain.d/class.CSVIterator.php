<?php
class CSVIterator extends SplFileObject {
	protected $firstlinetitle;
	protected $is_firstline;
	protected $names;
    /**
     * @param string $pathToFile
     * @param string $delimiter
     * @param string $fieldEnclosure
     * @param string $escapeChar
     */
    public function __construct($pathToFile, $delimiter=";", $fieldEnclosure='"', $escapeChar='\\', $firstlinetitle = false) {
        parent::__construct($pathToFile, 'r');
		$this->firstlinetitle = $firstlinetitle;
		$this->is_firstline   = true;
        $this->setFlags(SplFileObject::READ_CSV);
        $this->setCsvControl($delimiter, $fieldEnclosure, $escapeChar);
    }
    /**
     * @param array $names
     * @return CSV_Iterator
     */
    public function setColumnNames(array $names)
    {
        $this->names = $names;
        return $this;
    }
    public function current() {
        $row = parent::current();
		if ($this->firstlinetitle && $this->is_firstline) {
			$this->names = array_values($row);
			$this->is_firstline = false;
			parent::next();
			$row = parent::current();
		}

        if ($this->names) {
            if (count($row) != count($this->names)) {
                return null;
            } else {
                $row = array_combine($this->names, $row);
            }
        }
        return $row;
    }
    public function valid()
    {
        $current = $this->current();
        if ($this->names) {
            return count($current) == count($this->names);
        }
        return parent::valid();
    }
}